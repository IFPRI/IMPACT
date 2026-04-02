# IMPACT Model — Driver Assumptions Metadata

**Model version:** v4.1.4  
**Scenario:** SSP2-NoCC-NoCC  
**Generated:** April 2026

> Domain set elements are listed separately in `domain_sets.csv`.

---

## Table of Contents

- [BaseYearData](#-ase-ear-ata)
- [ClimateImpacts_Yield](#-limate-mpacts--ield)
- [Elasticities_Demand](#-lasticities--emand)
- [Elasticities_Supply](#-lasticities--upply)
- [ProductionGrowth_Area](#-roduction-rowth--rea)
- [ProductionGrowth_Yield](#-roduction-rowth--ield)
- [Socioeconomic](#-ocioeconomic)
- [Trade_Parameters](#-rade--arameters)
- [Demand](#-emand)
- [Supply](#-upply)

---

## BaseYearData

### `BaseCTY`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Loading parameter for cty-level entropy results |
| **Domain** | `JJJ`, `CTY`, `RIQ` |

### `BaseCTY_Fish`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Loading base year data for fish |
| **Domain** | `CTY`, `AC`, `RIQ` |

### `BaseFPU`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Loading parameter for FPU-level crop area-yield-QS |
| **Domain** | `J`, `FPU`, `LND`, `RIQFPU` |

### `BaseFPUL`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Loading parameter for FPU-level livestock number-yield-QS |
| **Domain** | `J`, `FPU`, `LVSYS`, `RIQFPU` |

## ClimateImpacts_Yield

### `RdInCCdelta`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | CC deltas adjusted to be a multiplier for IMPACT 3 |
| **Domain** | `gcm`, `rcp`, `CropModel`, `Technology`, `J`, `FPU`, `LND` |

## Elasticities_Demand

### `BFDmdPElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Biofuels Demand Price Elasticity |
| **Domain** | `C`, `CC`, `CTY` |

### `ExogFDDmdElasH`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous trend for household own-price elasticities for food demand |
| **Domain** | `C`, `H`, `CTY`, `YRS` |

### `ExogIncDmdElasH`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous trend for household income elasticities for food demand |
| **Domain** | `C`, `H`, `CTY`, `YRS` |

### `FdElasHX0`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Final Household food demand elasticities |
| **Domain** | `C`, `CC`, `H`, `CTY`, `YRS` |

### `FeedElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Livestock feed demand elasticity |
| **Domain** | `C`, `CC`, `CTY` |

### `IncDmdElasHX0`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Final Household income demand elasticities |
| **Domain** | `C`, `H`, `CTY`, `YRS` |

### `OthDmdIElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Other demand income elasticity |
| **Domain** | `C`, `CTY` |

### `OthDmdPElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Other demand price elasticity |
| **Domain** | `C`, `CC`, `CTY` |

## Elasticities_Supply

### `AnmlFeedElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Animal feed elasticity of supply |
| **Domain** | `J`, `C`, `CTY` |

### `AreaElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Area supply elasticities wrt to PNET |
| **Domain** | `JJJ`, `FPU`, `LND` |

### `AnmlElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Animal stock supply elasticities |
| **Domain** | `J`, `JJ`, `CTY` |

### `YldElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Yield elasticity wrt to land price |
| **Domain** | `JJJ`, `FPU`, `LND` |

### `YldElasWF`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Yield elasticity wrt to other input prices |
| **Domain** | `JJJ`, `FPU`, `LND`, `FCTR` |

### `QSElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Supply price elasticities |
| **Domain** | `J`, `J`, `CTY` |

### `QSElasC`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Supply price elasticities for inputs |
| **Domain** | `C`, `J`, `CTY` |

### `WFElas`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | FPU land demand elasticity wrt WFV |
| **Domain** | `J`, `FPU`, `FCTR` |

### `pop05`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Percent of the population that are between 0 and 5 (decimal) |
| **Domain** | `SSP`, `CTY`, `YRS` |

## ProductionGrowth_Area

### `areagr`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous 5-yr area growth by FPU and land type |
| **Domain** | `JJJ`, `FPU`, `LND`, `YRPER` |

### `anmlnumgrx0`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous growth rates for livestock herd size |
| **Domain** | `J`, `FPU`, `LVSYS`, `YRS` |

### `lndgr`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous 5-yr land growth by FPU and land type |
| **Domain** | `FPU`, `LND`, `YRPER` |

## ProductionGrowth_Yield

### `yldgr`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous 5-yr yield growth by FPU and land type |
| **Domain** | `JJJ`, `FPU`, `LND`, `YRPER` |

### `anmlyldgrx0`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous productivity growth for livestock yield |
| **Domain** | `J`, `FPU`, `LVSYS`, `YRS` |

### `yldbumpx0`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Additional bump growth to IPRs |
| **Domain** | `FPU`, `YRS` |

## Socioeconomic

### `RdInGDPSSP`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Total GDP for IMPACT |
| **Domain** | `MOD`, `PROJ`, `CTY`, `YRS` |

### `GDPSSPgr`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous annual GDP growth rates |
| **Domain** | `SCENARIO`, `CTY`, `YRS` |

### `RdInPOPSSP`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Total Population for IMPACT |
| **Domain** | `MOD`, `PROJ`, `CTY`, `YRS` |

### `POPSSPgr`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Exogenous annual population growth rates |
| **Domain** | `SCENARIO`, `CTY`, `YRS` |

## Trade_Parameters

### `RdInCSE`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Consumer subsidy equivalent from GFACTCOM |
| **Domain** | `CTY`, `C` |

### `PW00`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Base year world price |
| **Domain** | `C` |

### `RdInMM`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Marketing margins from GFACTCOM |
| **Domain** | `CTY`, `C` |

### `RdInMME`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Marketing margins for exports based on price wedge calculations |
| **Domain** | `C`, `CTY` |

### `RdInMMJ`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Marketing margins from farmgate based on price wedge calculations |
| **Domain** | `JJJ`, `CTY` |

### `RdInMMM`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Marketing margins for imports based on price wedge calculations |
| **Domain** | `C`, `CTY` |

### `RdInPSE`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Producer subsidy equivalent from GFACTCOM |
| **Domain** | `CTY`, `JJJ` |

### `TE`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Export taxes |
| **Domain** | `C`, `CTY` |

### `TM`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Import tariffs |
| **Domain** | `C`, `CTY` |

## Demand

### `WasteInt`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Household food waste multiplier |
| **Domain** | `C`, `CTY` |

### `RdInFeedReq`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Livestock feed requirements |
| **Domain** | `C`, `CTY`, `J` |

### `QBFInt2`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Biofuel demand multiplier |
| **Domain** | `C`, `CTY` |

### `QOthrInt2`

| Field | Value |
|---|---|
| **Type** | Parameter |
| **Description** | Other demand multiplier |
| **Domain** | `C`, `CTY` |

## Supply

*No symbols extracted for this folder.*

