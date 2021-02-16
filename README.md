# postgresql-simple-qq

Haskell quasiquoters for [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple).

## Features

* Compile-time syntax checking of PostgresSQL statements.
* Named placeholders. Use Haskell identifiers directly from within the SQL query.

```haskell
getEmployee :: Connection -> UUID -> Text -> IO Employee
getEmployee conn empId title = do
  let (q,vs) = [psql| SELECT *
                      FROM employees
                      WHERE employee_id = ?empId
                        AND title = ?title |]
  mkEmployee <$> query conn q vs
```

## Credits

Syntax checking is made possible by the excellent [postgresql-syntax](https://github.com/nikita-volkov/postgresql-syntax) package. Thank you!
