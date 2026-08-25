# Retrieve loaded app data

The documented way to access datasets loaded by \[load_app_data()\].
Replaces the per-name global accessor functions previously created by
\`create_data_accessors()\`.

## Usage

``` r
get_app_data(name = NULL)
```

## Arguments

- name:

  Optional. The name of a single dataset to return. If \`NULL\` (the
  default), the full named list is returned \*\*raw\*\* — undecorated,
  and without triggering any deferred loaders. Use
  \`get_app_data("\<name\>")\` to get a decorated dataset; the \`NULL\`
  form is for metadata such as \`names(get_app_data())\`, not for
  pulling datasets out with \`\[\[\`.

## Value

The requested dataset, or the full named list when \`name\` is \`NULL\`.
