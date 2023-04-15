## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

-----

## Responses to remarks I received from 1st submission

This is 2nd submission. As for the remarks I received from 1st submission: 

### 1

    Thanks, if there are references describing the methods in your package, 
    please add these in the Description field of your DESCRIPTION file in 
    the form
    authors (year) <doi:...>
    authors (year) <arXiv:...>
    authors (year, ISBN:...)
    with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

There are NO references describing the methods in this package. 

### 2

    Additionally, we see:
      Warning: Unexecutable code in man/set_collunit.Rd:
      notes = "my lake":
      Warning: Unexecutable code in man/set_dataset.Rd:
      notes = "my lake":
      Warning: Unexecutable code in man/toJSON-sites-method.Rd:
      Convert the: 
      Warning: Unexecutable code in man/toWide.Rd:
      Download all:

I have fixed the code so that it can be executed.


### 3

    Please add \value to .Rd files regarding exported methods and explain the functions results in the  documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
```
Missing Rd-tags in up to 204 .Rd files, e.g.:
     add_chronology-collunit-chronology-data.frame-method.Rd: \value
     add_chronology.Rd: \value
     as.data.frame-authors-method.Rd: \value
     as.data.frame-chronologies-method.Rd: \value
     as.data.frame-chronology-method.Rd: \value
     as.data.frame-collunit-method.Rd: \value
     ...
```
Added `@returns` to function with descriptions. This adds the `value` tag to .Rd files

### 4

    \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
    Does not seem necessary.
    Please unwrap the examples if they are executable in < 5 sec, or create additionally small toy examples to allow automatic testing.
    (You could also replace \dontrun{} with \donttest, if it takes longer than 5 sec to be executed, but it would be preferable to have automatic checks for functions. Otherwise, you can also write some tests.)

    Please replace \dontrun{} by \donttest{} or unwap the examples if they 
    can be executed in less than 5 sec per Rd-file.
    
Unwrapped some examples and replaced the `\dontrun{}` with a `\donttest{}` label. Most examples make calls to the Neotoma API and CANNOT use smaller toy datasets. Because of this, some examples take longer than 5 seconds and have to keep the `\donttest{}` label.


### 5

    Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
    Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
    
We do not write elements in the user's homespace.