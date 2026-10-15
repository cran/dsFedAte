
# dsFedAte

<!-- badges: start -->
<!-- badges: end -->

Server-side 'DataSHIELD' package for federated estimation of the average
treatment effect (ATE) across multiple studies distributed over separate
data nodes. `dsFedAte` is installed on the server side (Opal or Armadillo);
the analyst-side companion package is `dsFedAteClient`.

## Installation

```r
install.packages("dsFedAte")
```

## Example

`dsFedAte` is called indirectly: the analyst runs `dsFedAteClient::ds.EstimateAte()`
on the client, which dispatches the server-side aggregate function `localAteDS()`
on each data node. A full worked example is provided in the `federated_rct`
vignette of the client package:

```r
vignette("federated_rct", package = "dsFedAteClient")
```
