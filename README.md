# Mustache Templating

## Specifications

This library implements the API of mustache from:

  `http://mustache.github.com/mustache.5.html`


## Render Function

The render accepts either a Unicode character list or a UTF-8 binary
as the template.

The Context is a dictionary:

* keys are either atom() or string()
* the data items are:
    * Unicode character list (Variable, Partial)
    * UTF-8 binary (Variable, Partial)
    * Integer (Variable)
    * Float (Variable)
    * Function/1 argument is current Context (Variable, Partial)
    * Function/2 areguments Template and Render function (Section)
    * Dictionary in single element tuple (Section)
    * List of dictionaries in single element tuple (Section)
    * Property List `[{atom()|string(), any()}]` (Section)


## Notes

Erlang dictionaries are system specific and cannot reliably be detected,
at present dictionaries are wrapped in a single element tuple (`{dict()}`).
A dictionary list is of type: `[{dict()}]`.
