

module type INTELLIGENCE = sig
  val choose_action: Battle.state -> Battle.action
end


module User : INTELLIGENCE = struct
  let choose_action st = failwith "unimplemented"
end

module AI : INTELLIGENCE = struct
  
end