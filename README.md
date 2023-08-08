
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GPTscreenR

<!-- badges: start -->
<!-- badges: end -->

GPTscreenR is an R package that uses [OpenAI’s GPT large language
model](https://platform.openai.com/docs/guides/gpt) to screen titles and
abstracts for [scoping
reviews](https://en.wikipedia.org/wiki/Systematic_review#Scoping_reviews).

## Installation

You can install the development version of GPTscreenR from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("wilkox/GPTscreenR")
```

GPTscreenR is not currently available on CRAN.

## OpenAI API key

Using GPTscreenR requires an OpenAI API key, which you must store in an
R environmental variable called `OPENAI_API_KEY`. OpenAI charges fees to
use their API. To obtain and set up an API key:

1.  [Create an OpenAI
    account](https://platform.openai.com/signup?launch), if you don’t
    have one already.
2.  Once you have created and signed in to your account, go to the [View
    API Keys page](https://platform.openai.com/account/api-keys).
3.  Click on the ‘Create new secret key’ button.
4.  Give your new key a name, for example ‘GPTscreenR’, and click
    ‘Create secret key’.
5.  The page will display your new secret API key. You must copy the key
    now, as it will never be displayed again.
6.  Store your API key in an R environmental variable by adding the
    following line to your `.Renviron` file:
    `OPENAI_API_KEY=<your secret API key goes here>`. There are a few
    different ways to edit this file:
    1.  On any operating system, you can use the function
        `usethis::edit_r_environ()` from [the usethis
        package](https://usethis.r-lib.org) to open and edit your
        `.Renviron` file.
    2.  On macOS or Linux, you can create or edit a file in your home
        directory called `~/.Renviron`.
    3.  You can also make an `.Renviron` file for a particular project.
        I strongly recommend that you *do not* check this file into a
        version control system like git, as this means other people with
        access to the repository (including the public, if the
        repository is publicly accessible) can see your secret key.

To check that you have set the environmental variable successfully, open
a new R session and load GPTscreenR with `library(GPTscreenR)`. You
should see the following message:

`` ✔ The `OPENAI_API_KEY` environmental variable is set ``

If instead you see:

`` ✖ The `OPENAI_API_KEY` environmental variable is not set ``

this means the environmental variable has not been set correctly. Note
that this does not check whether or not you have stored a valid key.

## Screening sources

‘Source’ is the term used in GPTscreenR for any published scholarly
work, such as an article, review, or letter, which might be considered
for inclusion in a scoping review. There are three steps to use
GPTscreenR to help screen source titles and abstracts for inclusion.
First, you must write a description of your review, so that GPT knows
what sorts of sources should be included and excluded. Second, you must
prepare a data frame of your sources including their titles and
abstracts. Third, you must ask GPT to screen each source against your
review description.

### Review description

Begin by writing a description of your review and storing it in a
variable. This can be as simple as a few sentences describing the aims
of your review. However, the more detailed and specific your review
description, the better GPT is likely to perform in screening sources,
and if you are using GPT to screen sources in parallel with human
reviewers it should be given the same guidelines that are given to the
humans. If you have specific inclusion and exclusion criteria, for
example for certain source types or languages, these should be included.

The GPTscreenR package provides a function `review_description()` to
help in compiling a review description using the [Population, Concept,
and Context (PCC)
framework](https://journals.lww.com/jbisrir/Fulltext/2023/03000/Recommendations_for_the_extraction,_analysis,_and.7.aspx)
for scoping reviews. Here is an example of using `review_description()`
to generate a PCC review description:

``` r
alpaca_review_description <- review_description(
  objective = "This scoping review will examine the existing primary research
  on the role of therapy alpacas in enhancing the mental health and emotional
  well-being of elderly residential aged care facility residents",
  population = "Elderly people living in residential aged care facilities.
  'Elderly' is defined as 65 years of age or older.",
  context = "Residential aged care facilities that have used therapy alpacas as
  part of their programme of care for residents. 'Residential aged care
  facility' is defined as a residential setting intended as a long-term place
  of residence for elderly people which includes provision of support for
  activities of daily living (e.g. meal preparation, bathing, housekeeping) and
  nursing support (e.g. medication management). Such facilities will also
  typically offer other structured programmes and facilities to provide
  entertainment, diversion, and wellbeing. It excludes other residential
  settings intended for elderly people that do no provide daily living or
  nursing supports (e.g. independent living villages) or that are not long-term
  (e.g. hospitals or hospices).",
  concept = "The impact of therapy alpaca programmes on stress reduction,
  emotional well-being, mental health, overall life satisfaction, or similar
  outcomes for residents."
)
```

### Data frame of sources

Your sources should be stored in a data frame that includes columns
named ‘title’ and ‘abstract’. For example:

``` r
print(alpaca_sources)
```

| title                                                                                                                                                    | abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | include |
|:---------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|
| Alpaca My Bags and Leave: Therapy Alpaca Programmes May Improve Resident Retention and Self-Reported Quality of Life in Residential Aged Care Facilities | Private Residential Aged Care Facilities (RACFs) in Western countries often struggle to retain residents due to high rates of lifestyle dissatisfaction. This observational study investigated the effect of therapy alpaca programs on resident retention and self-reported quality of life (QOL) in private residential aged care facilities. Data were collected from a sample of elderly residents in two matched facilities in Sydney, Australia, one with an established therapy alpaca program and one without. Resident retention rates were compared over a 12-month period, and self-reported QOL was assessed using a standardized questionnaire. The results indicated that the facility with the therapy alpaca program had significantly higher resident retention rates than the comparison facility. Additionally, residents in the facility with the therapy alpaca program reported higher QOL scores related to emotional well-being and overall life satisfaction. The study suggests that implementing therapy alpaca programs may contribute to improved resident retention and QOL in residential aged care facilities. | TRUE    |
| …                                                                                                                                                        | …                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | …       |

### Screening sources against your review description

The main function of GPTscreenR is `screen_sources()`. This takes the
data frame of package sources and the review description, and uses GPT
to screen the source titles and abstracts. It returns a data frame that
includes GPT’s recommendation on whether to include or exclude the
source. By default, `screen_sources()` will create a file called
`sources_cache.rds` to cache the results of screening, which allows
screening to be split across multiple sessions and to recover from
interruptions.

``` r
alpaca_results <- screen_sources(alpaca_sources, alpaca_review_description)
```

| title                                                                                                                                                    | abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | include | GPT_conversation | GPT_recommendation |
|:---------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|:-----------------|:-------------------|
| Alpaca My Bags and Leave: Therapy Alpaca Programmes May Improve Resident Retention and Self-Reported Quality of Life in Residential Aged Care Facilities | Private Residential Aged Care Facilities (RACFs) in Western countries often struggle to retain residents due to high rates of lifestyle dissatisfaction. This observational study investigated the effect of therapy alpaca programs on resident retention and self-reported quality of life (QOL) in private residential aged care facilities. Data were collected from a sample of elderly residents in two matched facilities in Sydney, Australia, one with an established therapy alpaca program and one without. Resident retention rates were compared over a 12-month period, and self-reported QOL was assessed using a standardized questionnaire. The results indicated that the facility with the therapy alpaca program had significantly higher resident retention rates than the comparison facility. Additionally, residents in the facility with the therapy alpaca program reported higher QOL scores related to emotional well-being and overall life satisfaction. The study suggests that implementing therapy alpaca programs may contribute to improved resident retention and QOL in residential aged care facilities. | TRUE    | `GPT_mssg`       | INCLUDE            |
| …                                                                                                                                                        | …                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | …       | …                | …                  |

The output of `screen_sources()` is a data frame identical to the data
frame of sources but with two additional columns. ‘GPT_conversation’ is
a
[list-column](https://jennybc.github.io/purrr-tutorial/ls13_list-columns.html),
each element of which is a `GPT_mssg` object containing the full
conversation between the GPTscreenR packages and GPT. This can be useful
for understanding how GPT came to a particular recommendation. The
`GPT_recommendation` column contains either ‘INCLUDE’ or ‘EXCLUDE’,
representing GPT’s recommendation for each source.
