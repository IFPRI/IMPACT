# extract_gdx_metadata.R
# Set the three paths below, then source or run the script.

gams_file   <- "gdx2csv.gms"
gdx_file    <- "../../Global Outlook Report 2026/OutputFiles/Scenarios/SSP2-NoCC-NoCC.gdx"
output_md   <- "../description.md"
output_csv  <- "../DriverAssumptions/domain_sets.csv"

library(gamstransfer)

# ── 1. Parse GAMS file ────────────────────────────────────────────────────────
parse_gams_file <- function(path) {
    lines <- readLines(path, warn = FALSE)
    
    folder_order   <- character()
    folder_map     <- list()
    current_folder <- NA_character_
    
    for (line in lines) {
        line <- trimws(line)
        
        if (grepl("^\\*[A-Za-z]", line) && !grepl("^\\*(To be|Generation)", line)) {
            folder_candidate <- trimws(sub("^\\*", "", line))
            if (!grepl("\\s", folder_candidate)) {
                current_folder <- folder_candidate
                if (!(current_folder %in% folder_order)) {
                    folder_order <- c(folder_order, current_folder)
                    folder_map[[current_folder]] <- character()
                }
            }
        }
        
        if (grepl("^\\$call gdxdump", line) && !is.na(current_folder)) {
            m <- regmatches(line, regexpr("symb=([A-Za-z0-9_]+)", line))
            if (length(m) == 1) {
                symb <- sub("symb=", "", m)
                folder_map[[current_folder]] <- c(folder_map[[current_folder]], symb)
            }
        }
    }
    
    list(order = folder_order, map = folder_map)
}

cat("Parsing GAMS file:", gams_file, "\n")
parsed        <- parse_gams_file(gams_file)
folder_order  <- parsed$order
folder_map    <- parsed$map
total_symbols <- sum(lengths(folder_map))
cat("  Found", length(folder_order), "folders and", total_symbols, "symbols\n")

# ── 2. Load GDX ───────────────────────────────────────────────────────────────
cat("Loading GDX file:", gdx_file, "\n")
m         <- Container$new(gdx_file)
available <- tolower(m$listSymbols())
cat("  Loaded", length(available), "symbols from container\n")

all_symbols <- unique(unlist(folder_map))
missing     <- setdiff(tolower(all_symbols), available)
if (length(missing) > 0) {
    cat("  Symbols not found in GDX:\n")
    print(missing)
}

# ── 3. Helper: get actual cased name from container ───────────────────────────
get_actual_name <- function(container, symb_name) {
    all_names <- container$listSymbols()
    matched   <- all_names[tolower(all_names) == tolower(symb_name)]
    if (length(matched) == 0) return(NULL)
    matched[1]
}

# ── 4. Helper: extract domain set info ───────────────────────────────────────
extract_domain_set <- function(set_obj) {
    name <- tryCatch(set_obj$name, error = function(e) "?")
    desc <- tryCatch(
        if (nzchar(set_obj$description)) set_obj$description else "",
        error = function(e) ""
    )
    recs  <- tryCatch(set_obj$records, error = function(e) NULL)
    elems <- if (!is.null(recs) && nrow(recs) > 0) recs[[1]] else character()
    
    list(name = name, description = desc, elements = elems)
}

# ── 5. Collect all unique domain sets across all parameters ──────────────────
cat("Collecting domain sets...\n")
domain_sets_all <- list()  # name -> list(description, elements)

for (symb in all_symbols) {
    actual <- get_actual_name(m, symb)
    if (is.null(actual)) next
    
    sym <- tryCatch(m[actual], error = function(e) NULL)
    if (is.null(sym)) next
    
    dom <- tryCatch(sym$domain, error = function(e) NULL)
    if (is.null(dom) || length(dom) == 0) next
    
    for (set_obj in dom) {
        info <- tryCatch(extract_domain_set(set_obj), error = function(e) NULL)
        if (is.null(info)) next
        if (!(info$name %in% names(domain_sets_all))) {
            domain_sets_all[[info$name]] <- list(
                description = info$description,
                elements    = info$elements
            )
        }
    }
}

cat("  Found", length(domain_sets_all), "unique domain sets\n")

# ── 6. Write CSV ──────────────────────────────────────────────────────────────
csv_rows <- do.call(rbind, lapply(names(domain_sets_all), function(sname) {
    info  <- domain_sets_all[[sname]]
    elems <- if (length(info$elements) > 0) info$elements else NA_character_
    data.frame(
        set_name    = sname,
        description = info$description,
        element     = elems,
        stringsAsFactors = FALSE
    )
}))

write.csv(csv_rows, output_csv, row.names = FALSE)
cat("Domain sets CSV written to:", output_csv, "\n")

# ── 7. Build markdown ─────────────────────────────────────────────────────────
lines_out <- character()
add <- function(...) lines_out <<- c(lines_out, paste0(...))

add("# IMPACT Model — Driver Assumptions Metadata")
add("")
add("**Model version:** v4.1.4  ")
add("**Scenario:** SSP2-NoCC-NoCC  ")
add("**Generated:** ", format(Sys.time(), "%B %Y"))
add("")
add("> Domain set elements are listed separately in `domain_sets.csv`.")
add("")
add("---")
add("")
add("## Table of Contents")
add("")
for (folder in folder_order) {
    anchor <- tolower(gsub("[^a-z0-9]", "-", folder))
    add("- [", folder, "](#", anchor, ")")
}
add("")
add("---")
add("")

for (folder in folder_order) {
    symbols <- folder_map[[folder]]
    add("## ", folder)
    add("")
    
    if (length(symbols) == 0) {
        add("*No symbols extracted for this folder.*")
        add("")
        next
    }
    
    for (symb in symbols) {
        add("### `", symb, "`")
        add("")
        
        actual <- get_actual_name(m, symb)
        if (is.null(actual)) {
            add("> ⚠️ Symbol **", symb, "** not found in GDX.")
            add("")
            next
        }
        
        sym  <- tryCatch(m[actual], error = function(e) NULL)
        if (is.null(sym)) {
            add("> ⚠️ Could not load symbol **", symb, "**.")
            add("")
            next
        }
        
        desc <- tryCatch(
            if (!is.null(sym$description) && nzchar(sym$description)) sym$description else "*No description available*",
            error = function(e) "*No description available*"
        )
        
        sym_type <- class(sym)[1]
        
        dom <- tryCatch(sym$domain, error = function(e) NULL)
        domain_names <- if (!is.null(dom) && length(dom) > 0) {
            sapply(dom, function(d) tryCatch(d$name, error = function(e) "?"))
        } else {
            character()
        }
        
        add("| Field | Value |")
        add("|---|---|")
        add("| **Type** | ", sym_type, " |")
        add("| **Description** | ", desc, " |")
        
        if (length(domain_names) == 0) {
            add("| **Domain** | *scalar (no domain)* |")
        } else {
            add("| **Domain** | `", paste(domain_names, collapse = "`, `"), "` |")
        }
        add("")
    }
}

# ── 8. Write markdown ─────────────────────────────────────────────────────────
writeLines(lines_out, output_md)
cat("Markdown written to:", output_md, "\n")
cat("Done.\n")