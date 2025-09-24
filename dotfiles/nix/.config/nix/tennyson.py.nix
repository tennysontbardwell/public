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
    owner = "tennyson";
    repo = "tennyson.py";
    rev = "938d603"; # v0.0.4
    sha256 = "";
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
