#!/bin/bash

hlint && stack build 2>&1 | less
