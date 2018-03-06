module type InternalConfig = {
  let apolloClient: ApolloClient.generatedApolloClient;
};

module QueryFactory = (InternalConfig: InternalConfig) => {
  external castResponse : string => {. "data": Js.Json.t} = "%identity";
  [@bs.module] external gql : ReasonApolloTypes.gql = "graphql-tag";
  type query('a) = {
    .
    "query": string,
    "parse": Js.Json.t => 'a,
    "variables": Js.Json.t
  };
  module type QueryConfig = {
    type t;
    type vars;
    let makeWithVariables: vars => query(t);
  };
  module Create = (QueryConfig: QueryConfig) => {
    type response =
      | Loading
      | Loaded(QueryConfig.t)
      | Failed(string);
    type renderProp = {
      data: response,
      stale: bool
    };
    type state = {
      response,
      query: query(QueryConfig.t),
      stale: bool
    };
    type action =
      | Result(string)
      | Error(string);
    let sendQuery = (~client, ~query, ~reduce) => {
      let _ =
        Js.Promise.(
          resolve(
            client##query({
              "query": [@bs] gql(query##query),
              "variables": query##variables
            })
          )
          |> then_(value => {
               reduce(() => Result(value), ());
               resolve();
             })
          |> catch(_value => {
               reduce(() => Error("an error happened"), ());
               resolve();
             })
        );
      ();
    };
    let component =
      ReasonReact.reducerComponentWithRetainedProps("ReasonApollo");
    let make =
        (~client=InternalConfig.apolloClient, ~variables, ~render, _children) => {
      ...component,
      initialState: () => {
        let query = QueryConfig.makeWithVariables(variables);
        let response =
          switch (
            client##readQuery({
              "query": [@bs] gql(query##query),
              "variables": query##variables
            })
          ) {
          | response =>
            let parse = query##parse;
            Loaded(parse(response));
          | exception (Js.Exn.Error(_e)) => Loading
          };
        {response, query, stale: false};
      },
      retainedProps: variables,
      reducer: (action, state) =>
        switch action {
        | Result(result) =>
          let parse = state.query##parse;
          let typedResult = parse(castResponse(result)##data);
          ReasonReact.Update({
            ...state,
            response: Loaded(typedResult),
            stale: false
          });
        | Error(error) =>
          ReasonReact.Update({...state, response: Failed(error), stale: false})
        },
      willReceiveProps: ({state, reduce, retainedProps}) =>
        if (variables !== retainedProps) {
          sendQuery(
            ~client,
            ~query=QueryConfig.makeWithVariables(variables),
            ~reduce
          );
          {...state, stale: true};
        } else {
          state;
        },
      didMount: ({state, reduce}) => {
        sendQuery(~client, ~query=state.query, ~reduce);
        ReasonReact.NoUpdate;
      },
      render: ({state}) => render({data: state.response, stale: state.stale})
    };
  };
};
