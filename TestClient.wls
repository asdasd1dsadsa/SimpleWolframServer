(* #!/usr/bin/env wolframscript *)
rep = URLRead@HTTPRequest["http://49.234.88.108:8082/1", <|Method -> "POST", "Query" -> <|"expr"->"DateString[]"|>|>];
Print@rep["Body"]