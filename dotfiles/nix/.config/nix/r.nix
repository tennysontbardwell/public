{ pkgs, ... }:
let
  # d3scatter = pkgs.rPackages.buildRPackage {
  #   name = "d3scatter";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "jcheng5";
  #     repo = "d3scatter";
  #     rev = "aba6687f7af974a55ee07b52bea0ccbf110623e8";
  #     sha256 = "uwq265azdiGNlRVZ6xmMPdpREV3vxqSQ3wFi7HSiIDw=";
  #   };
  # };

  myRPackages = with pkgs.rPackages; [
    # dataframes
    dplyr
    tibble

    # plotting
    ggplot2
    patchwork # plot composition
    GGally
    ggridges
    hexbin
    sugrrants # calendar facets
    # ggstatsplot # stats on plots # r-RcppParallel is broken
    viridis # colors
    WeightedTreemaps # Voronoi treemaps
    ggnewscale
    hrbrthemes # themes
    # DAGitty # causal diags
    # ggpirate
    # https://github.com/clauswilke/ggisoband

    # data processing
    purrr
    tidyr
    janitor # cleanup
    forcats # factor order
    stringr # (tidyverse)

    # io
    rjson
    vvtableau # tableau comm
    readr
    ascii # org/md/etc export

    # interactive / html
    crosstalk
    htmlwidgets
    r2d3
    shiny
    # shinyr # TODO BROKEN
    shinyWidgets
    shinymodels
    shinypivottabler
    shinyjqui
    shinyepico
    shinypanels
    shinyaframe
    nextGenShinyApps
    cyjShiny
    DT
    plotly
    # d3scatter

    # 3d
    threejs

    # networks
    ggraph # ggplot for networks
    visNetwork
    dygraphs
    networkD3

    # stats
    corrr
    AER # econometrics
    invgamma
    broom

    # ml
    tidymodels
    tidyAML # automl
    h2o # automl
    lime # interpretability
    caret
    recipes # features
    lavaan # latent factors
    systemfit # ols
    # brms #  # r-RcppParallel is broken is broken
    tidybayes
    # rstan #  # r-RcppParallel is broken is broken
    torch

    # tutorials
    # qgshiny
    # shinyMolBio
    # stplanr

    # econ
    eurostat

    # spatial data
    leaflet
    leaflet_extras
    leaflet_extras2
    sf
    terra
    spData
    # spDataLarge
    mapedit
    elevatr # elevation
    graphhopper # routing (open source)
    tidygeocoder # address lookup
    rmapzen # alt address lookup

    # time series
    zoo
    xts # extend zoo
    fable

    # weather
    metR

    # misc
    remotes # dev package install

    # broken
    # rbokeh
  ];

  myREnv = pkgs.rWrapper.override{
    packages = myRPackages;
  };

  myRStudio = pkgs.rstudioWrapper.override{
    packages = myRPackages;
  };

in
{
  paths = [
    myREnv
    myRStudio
  ];
}
