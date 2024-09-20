# Questions
This package simplifies the handling of various types of quiz questions by consolidating them into a single `Question` type.
It supports both closed-ended questions, where users select from predefined options, and open-ended questions, where users provide answers in a text field.

The key advantage of this package is that the `Question` type does not use type parameters, allowing you to manage different question types without creating and maintaining multiple custom types.
This approach reduces complexity and helps you avoid the need to update your type definitions every time a new question type is added.
