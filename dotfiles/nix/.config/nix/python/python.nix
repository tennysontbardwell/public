{ lib, pkgs, ... }:
let
  python3 = pkgs.python3.override {
    self = python3;
    packageOverrides = pyfinal: pyprev: {
      snapshot-pyppeteer = pyfinal.callPackage ./snapshot-pyppeteer.nix { };
      tennyson = pyfinal.callPackage ./tennyson.py.nix { };
      rectified-flow = pyfinal.callPackage ./rectified-flow.py.nix { };
    };

  };

  myPythonEnv = python3.withPackages (
    ps: with ps; [
      tennyson
      rectified-flow

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
      websockets

      ### Data frames & io
      numpy
      pandas
      polars
      fastparquet
      pyarrow
      parquet # TODO wait for fix

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
      jsonpickle
      simple-term-menu

      ### api
      yfinance
      kaggle

      ## not FOSS / not mac
      # accelerate
      # bitsandbytes

      ### python build system
      conda
    ]
  );

  myPythonEnvWithQt = pkgs.symlinkJoin {
    name = "python-env-with-qt";
    paths = [ myPythonEnv ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/python \
        --prefix QT_PLUGIN_PATH : "${pkgs.qt5.qtbase.bin}/${pkgs.qt5.qtbase.qtPluginPrefix}"
    '';
  };
in
{
  paths = with pkgs; [
    myPythonEnvWithQt
    uv
    jetbrains.pycharm-community
    pipx
  ];
}
