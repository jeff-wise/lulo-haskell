#!/bin/bash

cd ../..
stack exec -- lulo-exe schemaschema html -f examples/schemaschema/dist/index.html --html-options examples/schemaschema/html-options.yaml
