# autoDocset
Working with haskell docsets for helm-dash, I often got too many results in libraries in which I was not interested in (for example, parts of base).
This script generates docsets from specified libraries, to use with dash/zeal/helm-dash

It requires the haddocset executable in the path. Example usage:

    stack exec autoDocset -- --docsetName myDocset --resolver lts-6.1 --libraries turtle --libraries foldl

This is, for the time being, intended for internal use in my projects. Read the source (it's very short).