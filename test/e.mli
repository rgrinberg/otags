module type END = sig 
  type end_t 
  val end_val : end_t
end

class ['a] int_value1 : object 
  val mutable r : 'a 
  method get : 'a
  method set : 'a -> unit
end

class type ['a] int_value2 = object 
  val mutable r : 'a 
  method get : 'a
  method set : 'a -> unit
end


class ['a] int_value3 : 'a -> object 
  val mutable r : 'a 
  method get : 'a
  method set : 'a -> unit
end
