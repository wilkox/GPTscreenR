# GPTscreenR (development version)

## Major changes

- OpenAI API code moved to the lemur package (https://github.com/wilkox/lemur)

## Minor changes

- Users can now set the model to use with the `OPENAI_MODEL` environmental
  variable
- Now using the httptest package for testing, all `.dry_run` arguments have
  been removed

# GPTscreenR 0.0.3

## Major changes

- Change prompt to multi-step, short chain-of-thought

## Minor changes

- Add a plea to contribute scoping review data
- Fix `screen_sources()` messaging when not subsetting

# GPTscreenR 0.0.2

## Major changes

- Streamline chain-of-thoughts to single prompt

## Minor changes

- Improve handling of malformed recommendations from GPT
- Fix typo in screening prompt 'search search'
- Fix bug in caching of sources with `screen_sources()`
- Explicit setting of model from an environmental variable
- Mention random subsetting, if applicable, in running count in
  `screen_sources()`

# GPTscreenR 0.0.1

- Initial release on GitHub.
