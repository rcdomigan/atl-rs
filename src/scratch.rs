        // Define => {
		//     if(match(rest(tag<Define>::value, capture_ptr(sym), capture(definition)),
		// 	         walker))
		// 	    NestAst nest_inner(builder);

		// 	builder.push_back(wrap<Define>());
		// 	builder.push_back(wrap(sym));

		// 	switch(definition.tag())
		// 	{
		// 		case tag<Ast>::value:
		// 		{
		// 			auto walk_def = walk_values(env, subex(definition));
		// 			_ast(env, builder, walk_def);
		// 			break;
		// 		}
		// 		default:
		// 		{ _atom(env, builder, *definition); }
		// 	}

		// 	return;
        // },
        Data => {
		    else if(match(rest_begins(tag<Data>::value), walker))
		    {
			    NestAst nest_data_dcl(builder);
			    builder.push_back(wrap<Data>());

			    auto data_type = wrap(Type(++new_types));
			    auto& data = expect<Symbol>(walker.value());
			    data.scheme.type = data_type;

			    builder.push_back(walker.value());

			    walker.next();
			    for(; walker.has_value(); walker.next())
			    {
				    if(walker.is_subex())
				    {
					    auto subex = walker.walk_subex();
					    auto& constructor = expect<Symbol>(walker.value());
					    walker.next();

					    auto type_builder = gc.ast_builder();
					    {
						    std::vector<NestAst> stacker;

						    for(; walker.has_value(); walker.next())
						    {
							    stacker.emplace_back(type_builder);

							    type_builder.push_back(function_constructor());

							    type_builder.push_back(walker.value());
						    }
						    type_builder.push_back(data_type);
					    }

					    constructor.scheme.type = type_builder.built();
					    builder.push_back(wrap(&constructor));
				    }
				    else if(is<Symbol>(walker.value()))
				    {
					    auto& sym = unwrap<Symbol>(walker.value());
					    sym.scheme.type = data_type;
					    builder.push_back(wrap(&sym));
				    }
				    else
				    {
					    throw WrongTypeError
						    ("data constructor declaration not Symbol or Ast");
				    }
			    }
		    }
        }
