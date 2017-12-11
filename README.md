

<div align="center">
  <img src="https://raw.githubusercontent.com/jeff-wise/lulo-haskell/master/docs/assets/logo.png" width="50%" />
</div>


<h4>
  View the <a href="https://jeff-wise.github.io/lulo-haskell/schemaschema/">Schema Docs</a>
</h4>

<h4>
  Other languages: <a href="https://github.com/jeff-wise/lulo-kotlin">Kotlin</a>
</h4>

------------------------------------------------------------------------

Lulo is a format for defining data types that may be shared between
between multiple programs.

You specify the structure of your data in *schema files* using 
**Algebraic Data Types** and **Refinement Types**.

Then you can:

  * Validate that a JSON / YAML document matches a schema (has the
      same structure / permitted values).
  * Automatically parse a JSON / YAML document that matches a schema.
    Parsing is flexible -- you can map the parsed data to any data structure.
  * Generate HTML documentation for a schema.

Because of the validation and automatic parsing that Lulo provides,
you can declare your data format in one location (a set of schema
files) and ensure that it is interpreted correctly in all locations.

Additionally, if your application allows customers to send data
through your API or create custom scripts, you only need to provide
the relevant schema files so that they can validate or generate
correct data to interact with your application. As a bonus, the schema
files come with automatically generated HTML documentation for your
clients.

#### Lulo is Defined With Lulo

Lulo can be defined using Lulo, and in fact, it is. Schemas are just 
documents, and Lulo parses them the same way that it parse any other document. 

This implies the existence of a *schema schema* which defines 
the format of all schemas.

The schema schema is hard-coded into the implementation because the 
schema schema is a schema, and we cannot parse a schema without the schema schema.

Since Lulo itself is a schema, it's HTML documentation can be
automatically generated -- it is self-documenting.

### Contents

- [Installation](#installation)
  - [Library](#library)
  - [Executable](#executable)
- [Getting Started](#getting-started)
    1. [Defining a Schema](#defining-a-schema)
    2. [Creating Documents](#creating-documents)
    3. [Parsing Documents](#parsing-documents)
    4. [Encoding Documents](#encoding-documents)
    5. [Generate HTML Documentation](#generate-html-documentation)
- [Schemas](#schemas)
  - [Types](#types)
    - [Primitives](#primitives)
    - [Products](#products)
    - [Sums](#sums)
  - [Constraints](#constraints)

## Installation

### Library

TODO

### Executable

TODO

## Getting Started

Let's get started by using Lulo to help manage data for a hypothetical 
quiz application. The application will have separate apps for iOS,
Android, and the web so that users can take and create quizzes on the
platform that's most convenient for them. In addition, there will be
a web server that facilitates sharing quizzes and other quiz-related 
content.

Our quiz application actually consists of four separate applications
even though they all have roughly the same business logic and data.
Lulo doesn't help us with the logic and procedures, but it is useful 
for managing the data.
Normally, you would have to (1) write code to define, parse, and encode
data for each application and then (2) maintain those four separate code
instances carefully, so that they don't diverge and cause errors. With
Lulo, we can define the data in one place, which saves us time now by
reducing the code we need to write and saves us problems in the
future, by ensuring a consistent view of data for all of our applications.

In particular, Lulo reduces the occurrence of two classes of errors
that commonly occur at application boundaries during common tasks such
as reading data from a file, parsing an API request body, or sending 
data to another component:

  1. **Parsing Errors** that result from data being interpreted
     incorrectly e.g. the parser of one component expects a field with
     the wrong name.
  2. **Encoding Errors** that occur when data is saved in the wrong
     format e.g. a field is named incorrectly

### 1. Defining a Schema

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
# Quiz
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
# Question
- type: sum_type
  sum_type:
    name: question
    label: Question
    description: A quiz question.
    group: 'Question'
    cases:
    - type: question_multiple_choice 
      description: A multiple choice question.
    - type: question_open_ended
      description: An open-ended question.
    - type: question_matching
      description: A matching question.
# Multiple Choice Question
- type: product_type
  product_type:
    name: question_multiple_choice
    label: Multiple Choice Question
    description: >
      A multiple choice question. The question consists of some text
      that describes the problem and a number of possible solutions.
    group: 'Question'
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
# Open-Ended Question
- type: product_type
  product_type:
    name: question_open_ended
    label: Open-Ended Question
    description: >
      An open-ended question. The answer may be any text.
    group: 'Question'
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
# Matching Question
- type: product_type
  product_type:
    name: question_matching
    label: Matching Question
    description: >
      An open-ended question. The answer may be any text.
    group: 'Question'
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
        to items on the left side.
```

### 2. Creating Documents

We just defined a schema to represent our quiz and its components. The
schema is equivalent to a type in a programming language -- it defines
the set of possible values that may exist. In this case, our quiz
schema defines the set of valid quizzes. We'll say our quiz matches
the schema if our quiz has the same structure and values defined by
the schema. Let's create a quiz about chess that matches our quiz
schema.

```yaml
name: Chess Quiz
description: A quiz about chess.
questions:
- type: question_multiple_choice
  question_multiple_choice:
    points: 1
    question: How many squares are on a chess board?
    choices:
    - '36'
    - '50'
    - '64'
    - '100'
- type: question_open_ended
  question_open_ended:
    points: 2
    question: 'What is your favorite chess opening and why?'
- type: question_matching
  question_matching:
    points: 2
    question: 'Match the piece to its movement pattern.'
    left_side: 
      - 'Rook'
      - 'King'
      - 'Bishop'
    right_side:
      - 'One square in any direction.'
      - 'Any number of squares diagonally.'
      - 'Any number of squares vertically or horizontally.'
```

We can use the `check` command from the Lulo executable to validate
that our quiz matches the schema.

```bash
stack exec lulo-exe -- check examples/quiz/schema.yaml examples/quiz/chess_quiz.yaml
```

It should print:

```bash
> Document is member of schema.
```

### 3. Parsing Documents

Suppose we want to write a program that can read quiz files like the
one we just defined and display them. We can use Lulo and the schema
defined in [step 1](#defining-a-schema) to automatically validate and
parse the quiz yaml files.

Lulo parses the yaml files into an intermediary representation. You
will have to write functions to map the intermediary value to your own
data types. The advantage of this method is that your data types are
not dependent on the yaml representation, or vice versa. In the future
there may be support for more automatic deserialization.

First, let's define the Haskell data types. For each data type, we'll
write an instance of the `FromDocument` typeclass which will take
a `Doc` and map it to our Haskell value. 

```haskell

data Quiz = Quiz
  { quizName        :: Text
  , quizDescription :: Text
  , questions       :: [Question]
  }

instance FromDocument Quiz where
  fromDocument (DocDict doc) = Quiz 
    <$> (atParser "name" doc >>= fromDocument)
    <*> (atParser "description" doc >>= fromDocument)
    <*> (atListParser "questions" doc >>= mapM fromDocument . listDocDocs)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


data Question = 
    QuestionMultipleChoice MultipleChoiceQuestion
  | QuestionOpenEnded OpenEndedQuestion
  | QuestionMatching MatchingQuestion

instance FromDocument Question where
  fromDocument doc = 
    case docCase doc of
      "question_multiple_choice" -> QuestionMultipleChoice <$> fromDocument doc
      "question_open_ended"      -> QuestionOpenEnded <$> fromDocument doc
      "question_matching"        -> QuestionMatching <$> fromDocument doc
      _                          -> Left $ ValueParseErrorUnknownCase $ 
        UnknownCaseError (docCase doc) "CustomType" (docPath doc)


data MultipleChoiceQuestion = MultipleChoiceQuestion
  { multipleChoiceQuestionPoints   :: Int
  , multipleChoiceQuestionQuestion :: Text
  , multipleChoiceQuestionChoices  :: [Text]
  }

instance FromDocument MultipleChoiceQuestion where
  fromDocument (DocDict doc) = MultipleChoiceQuestion
    <$> (atParser "points" doc >>= fromDocument)
    <*> (atParser "question" doc >>= fromDocument)
    <*> atTextListParser "choices" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


data OpenEndedQuestion = OpenEndedQuestion
  { openEndedQuestionPoints   :: Int
  , openEndedQuestionQuestion :: Text
  }

instance FromDocument OpenEndedQuestion where
  fromDocument (DocDict doc) = OpenEndedQuestion
    <$> (atParser "points" doc >>= fromDocument)
    <*> (atParser "question" doc >>= fromDocument)
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)


data MatchingQuestion = MatchingQuestion
  { matchingQuestionPoints    :: Int
  , matchingQuestionQuestion  :: Text
  , matchingQuestionLeftSide  :: [Text]
  , matchingQuestionRightSide :: [Text]
  }

instance FromDocument MatchingQuestion where
  fromDocument (DocDict doc) = MatchingQuestion
    <$> (atParser "points" doc >>= fromDocument)
    <*> (atParser "question" doc >>= fromDocument)
    <*> atTextListParser "left_side" doc
    <*> atTextListParser "right_side" doc
  fromDocument doc           = Left $ ValueParseErrorUnexpectedType $ 
    UnexpectedTypeError DocDictType (docType doc) (docPath doc)

```

Now, we can use Lulo to parse quizzes. First, we'll parse the schema
file and keep it in scope so we don't have to parse it again. Then we
use the schema to parse the document:

```haskell

quizSchemaFilePath :: FilePath
quizSchemaFilePath = "/quiz_app/schemas/quiz.yaml"

quizDocumentFilePath :: FilePath
quizDocumentFilePath = "/quiz_app/quizzes/chess_quiz.yaml"

main :: IO ()
main = do
  eSchema <- parseSchemaFile quizSchemaFilePath
  case eSchema of
    Left err -> putStrLn $ "Could not parse quiz schema:\n\n" <> show err
    Right schema -> do
      eQuiz <- parseDocumentFileValue quizDocumentFilePath schema
      case eQuiz of
        Left  err  -> putStrLn $ "Could not parse quiz:\n\n" <> show err
        Right quiz -> doSomething quiz 

```

### 4. Encoding Documents

Coming Soon!

### 5. Generating HTML Documentation

Now, that our application can import and export quizzes, we'd like to
allow our users to create their own quiz files. We'd also like to
create a community for our quiz app by supporting third-party
developers. Perhaps some developers would like to create some
utilities to manage or create quizzes that are compatible with our
application.

In order to faciliate external use of the quiz files, we need
comprehensive and clear documentation. Fortunately, all of the
information is already in the schema file. Unfortunately, it's not
easy to read a schema file, since it's fairly verbose and only one
step removed from plaintext. 

We will use Lulo's HTML generation capability to create nice
documentation for our quiz app. We can do so with the `html` command
in the Lulo executable. If we run `--help` on the `html` command, the
executable with display the options. Note: the command takes in an html
options file that will control the output of the generated HTML. This
file provides a lot of flexibility, should you need it, and is
intended to avoid the scenario that you need to write your own HTML
generation code.


The quiz example in the examples directory contains an html options
file along with a default stylesheet. To create the documentation, run
the following command:

```bash
stack exec -- lulo-exe html -f examples/quiz/dist/index.html --html-options examples/quiz/html-options.yaml
```

## Schemas

### Types

TODO

#### Primitives

TODO

#### Products

TODO

#### Sums

TODO

### Constraints

TODO


[lulo-html-documentation]: https://jeff-wise.github.io/lulo-haskell/schemaschema/
[lulo-kotlin]: https://github.com/jeff-wise/lulo-kotlin
