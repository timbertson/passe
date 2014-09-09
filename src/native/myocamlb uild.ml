dispatch begin function
  | After_rules ->
      Pathname.define_context "client" ["passe"; "client"]
  | _ -> ()
end
