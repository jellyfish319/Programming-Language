let calc = fun o x y ->
   if o = '+' then x + y
   else if o = '-' then x - y
   else if o = '*' then x * y
   else if o = '/' then if y = 0 then failwith"Divide-by-Zero"
      else x / y
   else failwith "Unsupported operation"