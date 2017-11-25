

<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/lulo-haskell/master/docs/assets/logo.png" width="60%" />
</div>


<h3 align="left">
  View the <a href="https://jeff-wise.github.io/lulo-haskell/schemaschema/">Schema Docs</a>

  <span align="right">
    <a href="https://github.com/jeff-wise/lulo-kotlin">Kotlin</a>
  </span>

</h3>


------------------------------------------------------------------------

Lulo is a data specification format. It allows you define *schemas*. With schemas, 
you can define the structure of your data with a collection of 
**Algebraic Data Types**. Additionally, schemas may contain constraints, 
sometimes called **Refinement Types**, on those types in 
order to precisely enforce the possible values of the data.

Then you can:

  1. Validate that a JSON / YAML document matches a schema.
  2. Automatically parse a JSON / YAML document that matches a schema.
     Parsing is flexible -- you can map the parsed data to any data structure.
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

> #### Lulo is Defined With Lulo
>
> Lulo can be defined using Lulo, and in fact, it is. Schemas are just 
> documents, and Lulo parses them the same way that it parse any other document. 
> This implies the existence of a **schema schema** which defines 
> the format of all schemas.
>
> The schema schema is hard-coded into the implementation because the 
> schema schema is a schema, and we cannot parse a schema without the schema schema.
>
> Since Lulo itself is a schema, it's HTML documentation can be
> automatically generated -- it is self-documenting.

### Contents

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

Let's get started with an example. We are going to define some basic
data types for software that manages quizzes or tests. Our software
will have both iOS and Android apps as well as a web application, so
users can use our application however they like. We also have a server
component that provides functionality for sharing quizzes among the users.

With Lulo, we can define our data in one place -- in a collection of
schema files -- and guarantee that each component in our quiz platform
interprets that data in the exact same way. This works because Lulo
can parse JSON or YAML documents based on the schema file, including
with specific constraints such as this value must be *greater than 0*.
Therefore, each component will be using the exact same data. This
prevents the two classes of errors that occur often at component 
boundaries when doing common tasks such as reading data from a file, 
parsing an API request body, or sending data to another component:
 
  1. **Parsing Errors** that result from data being interpreted
     incorrectly e.g. the parser of one component expects a field with
     the wrong name.
  2. **Encoding Errors** that occur when data is saved in the wrong
     format e.g. a field is named incorrectly

Additionally, if the data format is changed, each component only 
needs the updated schema. No code modification is necessary. 

### 1. Define a Schema

Let's define our quiz data. The schema defines the structure and
possible values of our data. It also contains a lot of metadata. The
metadata is very useful for documenting various aspects of the data
format such as:

  * Introduce why the data is relevant and when it should be used.
  * Why the data is structured the way it is.
  * Explain the purpose of each type and each field.
  * How to use the data e.g. example instances of data.

Many of the metadata fields support Markdown, as shown in the example.
The markdown is used in the generated HTML documentation, so you can
created comprehensive documentation for your data directly in the
schema file.

```yaml
version: '0.1'
metadata:
  name: Quiz
  authors:
  - name: 'Author'
description:
  overview: |+
    Quizzes are an assessment of a person's knowledge. Each quiz has
    a maximum number of points. Each person that completes a quiz is
    assigned some number of points between 0 and the maximum.

    A quiz may contain any number of questions. Question may presented
    in the following formats:
      
      * Multiple Choice
      * Open-Ended
      * Fill in the Blank

root_type: quiz
types:
- type: product_type
  product_type:
    name: quiz  
    label: Quiz
    description: >
      A quiz is a collection of questions. Each question has a point
      value. Each quiz has a score which is the total point value of all the
      questions a user has answered correctly.
    group: Quiz
    fields:
    - name: name
      type: 
        type: prim_type
        prim_type: string
      presence: required
      description: The name of the quiz.
    - name: description
      type: 
        type: prim_type
        prim_type: string
      presence: required
      description: The quiz description.
    - name: questions
      type: 
        type: custom_coll_type
        custom_coll_type: question
      presence: required
      description: The quiz questions.
- type: sum_type
  sum_type:
    name: question
    label: Question
    description: A quiz question.
    group: Question
    cases:
    - type: question_multiple_choice 
      description: A multiple choice question.
    - type: question_open_ended
      description: An open-ended question.
    - type: question_matching
      description: A matching question.
- type: product_type
  product_type:
    name: question_multiple_choice
    label: Multiple Choice Question
    description: >
      A multiple choice question. The question consists of some text
      that describes the problem and a number of possible solutions.
    group: 'Question: Multiple Choice'
    fields:
    - name: points
      type: 
        type: prim_type
        prim_type: number
      presence: required  
      description: >
        The number of points this question is worth when answered
        correctly.
    - name: question
      type:
        type: prim_type
        prim_type: string
      presence: required
      description: >
        The question the quiz taker must answer. The question will
        have multiple answers to choose from.
    - name: choices
      type: 
        type: prim_coll_type
        prim_coll_type: string
      presence: required
      description: >
        The possible answers to the question.
- type: product_type
  product_type:
    name: question_open_ended
    label: Open-Ended Question
    description: >
      An open-ended question. The answer may be any text.
    group: 'Question: Multiple Choice'
    fields:
    - name: points
      type: 
        type: prim_type
        prim_type: number
      presence: required  
      description: >
        The number of points this question is worth when answered
        correctly.
    - name: question
      type:
        type: prim_type
        prim_type: string
      presence: required
      description: >
        The question the quiz taker must answer. The answer may be
        anything.
- type: product_type
  product_type:
    name: question_matching
    label: Matching Question
    description: >
      An open-ended question. The answer may be any text.
    group: 'Question: Multiple Choice'
    fields:
    - name: points
      type: 
        type: prim_type
        prim_type: number
      presence: required  
      description: >
        The number of points this question is worth when answered
        correctly.
    - name: question
      type:
        type: prim_type
        prim_type: string
      presence: required
      description: >
        The question the quiz taker must answer. The answer may be
        anything.
    - name: left_side
      type: 
        type: prim_coll_type
        prim_coll_type: string
      presence: required
      description: >
        The list of options on the left side to match to items on the
        right side.
    - name: right_side
      type: 
        type: prim_coll_type
        prim_coll_type: string
      presence: required
      description: >
        The list of options on the right side that need to be matched
        to items on the right side.
```

### 2. Parse Documents 

### 3. Generate HTML Documentation

## Schemas

### Types

#### Primitives

#### Products

#### Sums

### Constraints


[lulo-html-documentation]: https://jeff-wise.github.io/lulo-haskell/schemaschema/
[lulo-kotlin]: https://github.com/jeff-wise/lulo-kotlin
