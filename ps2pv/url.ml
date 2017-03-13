open Pcre

type url = {
  mutable protocol: string;
  mutable domain_name: string;
  mutable port_number: int;
  mutable query_path: string;
  mutable query_string: (string * string) list;
  mutable hashtag: string
}

let parse_url =
  let re = regexp "((?<proto>[a-z]+):)?(//(?<domain>([a-z][a-z0-9]*\\.)*[a-z][a-z0-9]*)(:(?<port>[0-9]+))?/?)?(?<path>[a-z0-9/.-]*)?([?](?<params>([a-z0-9]+(=[a-z0-9+/%-]*)?&?)*))?(#(?<hash>[a-z0-9+/=-]*))?" in
  let sub m s = try get_named_substring re s m with _ -> "" in 
  fun s -> let m = exec ~rex:re s in
  {
    protocol = sub m "proto";
    domain_name = sub m "domain";
    port_number = (try int_of_string (sub m "port") with _ -> match sub m "proto" with "https" -> 443 | "ftp" -> 21 | _ -> 80);
    query_path = sub m "path";
    query_string = List.map (fun p->let l = split ~pat:"=" ~max:2 p in (List.hd l, try List.hd (List.tl l) with _ -> "")) (split ~pat:"&" (sub m "params"));
    hashtag = sub m "hash"
  }
