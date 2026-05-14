{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  wheel,
  accelerate,
  clip,
  diffusers,
  matplotlib,
  numpy,
  packaging,
  pillow,
  plotly,
  scikit-learn,
  scipy,
  sympy,
  timm,
  torch,
  torchvision,
  tqdm,
  ipykernel,
  nbformat,
}:

buildPythonPackage rec {
  pname = "rectified-flow";
  version = "1.0.1";

  src = fetchFromGitHub {
    owner = "lqiang67";
    repo = "rectified-flow";
    rev = "1ce623904d3627edc4349646d53f8cc8e603e7f7"; # v1.0.1
    sha256 = "sha256-+CiBoq3JMYQLYj0YnctvVu8Y1ehp8HkRHA8rHdcI6OA=";
  };

  postPatch = ''
    sed -i '25s/openai-clip==1.0.1/clip==1.0.0/' setup.py
  '';

  doCheck = true;

  pythonImportsCheck = [ "rectified_flow" ];

  propagatedBuildInputs = [
    accelerate
    clip
    diffusers
    matplotlib
    numpy
    packaging
    pillow
    plotly
    scikit-learn
    scipy
    sympy
    timm
    torch
    torchvision
    tqdm
    ipykernel
    nbformat
  ];

  # specific to buildPythonPackage, see its reference
  pyproject = true;
  build-system = [
    setuptools
    wheel
  ];
}
