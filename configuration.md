# check.fnl configuration

To configure check.fnl a Fennel script returning a table holding the configuration can be used. The following is a minimal example:
```
{
  :color false
  :max-line-length 100
  :checks {
    :style-delimiters false
    :if->when false
  }
}
```
Call check.fnl with the ``-c`` option to specify the config file.

## options
option | type | default value | description
--- | --- | --- | ---
``color`` | boolean | ``true`` | 
``max-line-length`` | integer | ``80`` | 
``checks`` | table | ``{}`` | use this table to enable or disable checks