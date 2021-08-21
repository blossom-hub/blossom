module Helpers

open Types

let splitAccounts account =
  let f (a: string) = a.Split(':') |> List.ofArray
  match account with | Account ms       -> f ms, None
                     | Account2 (ms, v) -> f ms, Some v

let joinAccounts elts virt =
  let stub = String.concat ":" elts
  match virt with | Some v -> Account2 (stub, v)
                  | None -> Account stub

let dropVirtualAccount = function | Account2 (account, _) -> Account account | a -> a

let (|GetAccount|) = function | Account account -> account | Account2 (account, _) -> account

let (|GetVirtualAccount|) = function | Account _ -> None | Account2 (_, virt) -> Some virt

let (|FlattenAccount|) includeVirtual account = 
  match account with | Account account -> account
                     | Account2 (account, virt) -> if includeVirtual then account + "/" + virt else account
