{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  wheel,
  boto3,
  click,
  hvac,
  psutil,
  pytest,
}:

buildPythonPackage rec {
  pname = "tennyson";
  version = "0.0.4";

  src = fetchFromGitHub {
    owner = "tennysontbardwell";
    repo = "tennyson.py";
    rev = "45289fa69aa9c81f56ad95fd5abf6758f95f8a0a"; # v0.0.4
    sha256 = "sha256-OHScP5Xq+dNY9541Hb9l+zg/1hU28Q0uooCQxpLO9bw=";
  };

  doCheck = true;

  pythonImportsCheck = [ "tennyson" ];

  propagatedBuildInputs = [
    boto3
    click
    hvac
    psutil
  ];

  # specific to buildPythonPackage, see its reference
  pyproject = true;
  build-system = [
    setuptools
    wheel
    pytest
  ];
}
