Needs["DatabaseLink`"]
$cacheFile = "/tmp/cache.hsqldb";
$cache = OpenSQLConnection[JDBC["HSQL(Standalone)", $cacheFile]]

openCacheConnection[] :=
  $cache = OpenSQLConnection[JDBC["HSQL(Standalone)", $cacheFile]]

closeCacheConnection[] :=
  CloseSQLConnection[$cache]

createCache[] :=
  SQLExecute[$cache,
    "CREATE TABLE cached_values (x float, y float, f float)
     ALTER TABLE cached_values ADD CONSTRAINT pk_cached_values PRIMARY KEY (x, y)"
  ]

saveCachedValue[x_, y_, value_] :=
  ( SQLExecute[$cache,
      "INSERT INTO cached_values (x, y, f) VALUES (?, ?, ?)", {x, y, value}
    ]
  ; value
  )

loadCachedValue[x_, y_] :=
  SQLExecute[$cache,
    "SELECT f FROM cached_values WHERE x = ? AND y = ?", {x, y}
  ] /. {{{v_}} :> v, {} :> $Failed}

replaceCachedValue[x_, y_, value_] :=
  SQLExecute[$cache,
    "UPDATE cached_values SET f = ? WHERE x = ? AND y = ?", {value, x, y}
  ]
