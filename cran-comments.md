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

-----

## Responses to remarks I received from 2nd submission

This is 3rd submission. As for the remarks I received from 2nd submission: 

### 1

    Please provide a link to the used webservices (Neotoma Database) to the 
    description field of your DESCRIPTION file in the form <http:...> or <https:...>
    with angle brackets for auto-linking and no space after 'http:' and
    'https:'.
    
Fixed.

### 2

    Please add \value to .Rd files regarding exported methods and explain 
    the functions results in the documentation. Please write about the 
    structure of the output (class) and also what the output means. (If a
    function does not return a value, please document that too, e.g.
    \value{No return value, called for side effects} or similar)
    Missing Rd-tags:
      chronologies-collunit-method.Rd: \value
      chronologies-collunits-method.Rd: \value
      chronologies-site-method.Rd: \value
      chronologies-sites-method.Rd: \value
      collunits-site-method.Rd: \value
      collunits-sites-method.Rd: \value
      datasets-collunit-method.Rd: \value
      datasets-collunits-method.Rd: \value
      datasets-site-method.Rd: \value
      datasets-sites-method.Rd: \value
      get_contacts.Rd: \value
      get_manual.Rd: \arguments
      get_publications.numeric.Rd: \value
      get_publications.publication.Rd: \value
      pipe.Rd: \arguments
      specimens-collunit-method.Rd: \value
      specimens-collunits-method.Rd: \value
      specimens-site-method.Rd: \value

All of the \value tags have been added. For the `pipe.Rd` function, we use the code from the [purrr library](https://github.com/tidyverse/purrr/blob/3b5add2db99a35ec1392ad23dc021b7ccadbbbbb/R/reexport-pipe.R)
We do not see any arguments being added into that function. In `magrittr`, there are two placeholders but they are used in the body of the `pipe` function - that is not our case.

As for the `get_manual.Rd` function, there are no parameters that the user needs to pass, it is a static function. I cannot find information on how to document this kind of "parameters".

### 3

    \dontrun{} should only be used if the example really cannot be executed
    (e.g. because of missing additional software, missing API keys, ...) by
    the user. That's why wrapping examples in \dontrun{} adds the comment
    ("# Not run:") as a warning for the user. Does not seem necessary.
    Please replace \dontrun with \donttest.
    
    get_datasets.numeric.Rd: Please put functions which download data in
    \donttest{}. ☑ Done
    get_manual.Rd: if(interactive()) is sufficient. You don't need to wrap
    it in \dontrun{} as well. ☑ Done
    specimens-sites-method.Rd: I believe the same as for 
    get_datasets.numeric.Rd. ☑ Done
    
Done as suggested