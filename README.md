

<p align="center">[Lulo Schema Format][lulo-html-documentation]</p>

------------------------------------------------------------------------

Lulo is a data specification format. It allows you define *schemas*. With schemas, 
you can define the structure of your data with a collection of 
**Algebraic Data Types**. Additionally, schemas may contain constraints, 
sometimes called **Refinement Types**, on those types in 
order to precisely enforce the possible values of the data.

Then you can:

  1. Validate that a JSON / YAML document matches a schema.
  2. Automatically parse a JSON / YAML document that matches a schema.
  3. Generate HTML documentation for a schema.

With Lulo you can declare your data types in schema files and use the
same data in different parts of your application. The data will always
be the same because the parsing is done automatically according to the
types defined in the schemas. 

If your application provides an open
data format for interfacing with an API or creating custom scripts, then 
clients will be able to create their own programs and use your data by 
just using Lulo and the data schemas. Plus, clients can use the 
generated HTML documentation as a guide.

##### How Does It Work?

Lulo can be defined using Lulo, and in fact, it is. Schemas are just 
documents, and Lulo parses them the same way that it parse any other document. 
This implies the existence of a **schema schema** which defines 
the format of all schemas.

The schema schema is hard-coded into the implementation because the 
schema schema is a schema, and we cannot parse a schema without the schema schema.

Since Lulo itself is a schema, it's HTML documentation can be
automatically generated -- it is self-documenting.

- [Getting Started](#getting-started)
  - [Define a Schema](#define-a-schema)
  - [Parse Documents](#parse-documents)
  - [Generate HTML Documentation](#generate-html-documentation)
- [Schemas](#schemas)
  - [Types](#types)
    - [Primitives](#primitives)
    - [Products](#products)
    - [Sums](#sums)
  - [Constraints](#constraints)


## Getting Started

### Define a Schema

### Parse Documents 

### Generate HTML Documentation

## Schemas

### Types

#### Primitives

#### Products

#### Sums

### Constraints


[lulo-html-documentation]: https://jeff-wise.github.io/lulo-haskell/schemaschema/
