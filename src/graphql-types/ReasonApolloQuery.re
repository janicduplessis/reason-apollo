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
    type state = {
      response,
      query: query(QueryConfig.t)
    };
    type action =
      | Result(string)
      | Error(string);
    let sendQuery = (~query, ~reduce) => {
      let _ =
        Js.Promise.(
          resolve(
            InternalConfig.apolloClient##query({
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
    let make = (~variables, children) => {
      ...component,
      initialState: () => {
        response: Loading,
        query: QueryConfig.makeWithVariables(variables)
      },
      retainedProps: variables,
      reducer: (action, state) =>
        switch action {
        | Result(result) =>
          let parse = state.query##parse;
          let typedResult = parse(castResponse(result)##data);
          ReasonReact.Update({...state, response: Loaded(typedResult)});
        | Error(error) =>
          ReasonReact.Update({...state, response: Failed(error)})
        },
      willReceiveProps: ({state, reduce, retainedProps}) =>
        if (variables !== retainedProps) {
          sendQuery(~query=QueryConfig.makeWithVariables(variables), ~reduce);
          state;
        } else {
          state;
        },
      didMount: ({state, reduce}) => {
        sendQuery(~query=state.query, ~reduce);
        ReasonReact.NoUpdate;
      },
      render: ({state}) => children(state.response)
    };
  };
};
