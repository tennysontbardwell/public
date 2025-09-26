{ lib, pkgs, ... }:
let
  python3 = pkgs.python3.override {
    self = python3;
    packageOverrides = pyfinal: pyprev: {
      snapshot-pyppeteer = pyfinal.callPackage ./snapshot-pyppeteer.nix { };
      tennyson = pyfinal.callPackage ./tennyson.py.nix { };
    };
  };

  myPythonEnv = python3.withPackages (
    ps: with ps; [
      tennyson

      ### misc/basic
      requests
      lxml
      beautifulsoup4
      pypdf
      tqdm
      pip
      aiohttp
      scrapy
      pipe

      ### Data frames & io
      numpy
      pandas
      polars
      fastparquet
      pyarrow
      # parquet TODO wait for fix

      ### ML
      jupyter
      jupyterlab
      scikit-learn
      scikit-misc
      spyder-kernels
      scipy

      ### plotting
      matplotlib
      bokeh
      tkinter
      pyqt5
      pyqt6
      opencv-python-headless
      plotnine
      altair
      vega_datasets
      pyecharts
      seaborn
      snapshot-pyppeteer
      # pyGAM
      # econML
      # parqv

      # LSP
      python-lsp-server
      yapf

      ### AI
      openai
      torch
      transformers
      torchvision
      torchaudio
      huggingface-hub
      # vllm

      ### Misc
      yfinance
      jsonpickle
      simple-term-menu

      ## not FOSS / not mac
      # accelerate
      # bitsandbytes
    ]
  );
in
{
  paths = with pkgs; [
    myPythonEnv
    uv
    jetbrains.pycharm-community
    pipx
  ];
}
