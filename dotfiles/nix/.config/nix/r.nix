{ pkgs, unstable-pkgs, ... }:
let
  myRPackages = with pkgs.rPackages; [
    dplyr
    forcats
    ggplot2
    htmlwidgets
    purrr
    readr
    stringr
    tibble
    tidyr
    xts
    ggridges
    viridis
    GGally
    AER
    r2d3
    dygraphs
    leaflet
    visNetwork
    DT
    ascii
    hexbin
    threejs
    networkD3
    tidyAML
    WeightedTreemaps
    corrr
    caret
    lime
    h2o
    recipes
    tidymodels
    janitor
    rjson
    hrbrthemes
    plotly
    vvtableau
    ggraph
    brms
    lavaan
    systemfit

    # broken
    # rbokeh
  ];

  myREnv = pkgs.rWrapper.override{
    packages = myRPackages;
  };

  myRStudio = unstable-pkgs.rstudioWrapper.override{
    packages = myRPackages;
  };

in
{
  paths = [
    myREnv
    myRStudio
  ];
}
