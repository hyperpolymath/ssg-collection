---
title: Lab Notebook Example
date: 2025-01-18
---

# Welcome to Lab Notebook

This example demonstrates the SciLab-powered scientific publishing system.

## Features

- Embedded SciLab code execution
- Matrix computation documentation
- Reproducible scientific results
- Mathematical notation support

## Matrix Example

Let's compute the eigenvalues of a simple matrix:

```scilab
A = [4 1; 2 3];
disp("Matrix A:");
disp(A);

// Compute eigenvalues
e = spec(A);
disp("Eigenvalues:");
disp(e);
```

## Signal Processing

A simple sinusoidal signal analysis:

```scilab
t = 0:0.01:2*%pi;
y = sin(t) + 0.5*sin(3*t);
disp("Signal statistics:");
disp("Mean: " + string(mean(y)));
disp("Std: " + string(stdev(y)));
```

## Linear Regression

Fitting a line to data points:

```scilab
x = [1 2 3 4 5]';
y = [2.1 3.9 6.2 7.8 10.1]';

// Least squares fit
coef = x \ y;
disp("Slope coefficient: " + string(coef));
```

## Build Instructions

```bash
# Initialize a new lab notebook
scilab-cli -f src/labnote.sce -- init

# Build the notebook (executes all embedded code)
scilab-cli -f src/labnote.sce -- build
```

## Scientific Publishing

Lab Notebook bridges the gap between computation and documentation:

- Write in Markdown
- Embed SciLab for computations
- Results rendered alongside explanations
- Fully reproducible scientific documents
