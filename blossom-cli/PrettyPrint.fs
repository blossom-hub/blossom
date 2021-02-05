module PrettyPrint

open System
open System.IO

open Journal
open FParsec
open Types
open Reports

type FileFormat = Blossom | Beancount | Ledger | Knut

let blossom2beancount outputfn journal =
  // header
  let header = [
    $"option \"title\" \"{journal.Meta.Name}\""
    $"option \"operating_currency\" \"USD\""
    $"option \"name_assets\" \"Asset\""
    $"option \"name_expenses\" \"Expense\""
    $"option \"name_liabilities\" \"Liability\""
    $"option \"name_expenses\" \"Expense\""
  ]

  // accounts
  let safeAccount (acc : string) = acc.Replace(" ", "-").Replace("/", "-")

  let date0 = journal.Register |> Map.toList |> List.map fst |> List.min
  let (MetaResultSet accountList) = meta id Accounts journal
  let accounts = [for account in accountList
                    do $"""{date0.ToString("yyyy-MM-dd")} open {safeAccount account}"""]

  // entries
  let rs = journal.Register |> Map.toList
  let f (dt: DateTime) es =
    let dts = dt.ToString("yyyy-MM-dd")
    let g e =
      let s1 = match e.Payee with | Some x -> x | None -> e.Narrative
      let s2 = match e.Payee with | Some x -> e.Narrative | None -> ""
      let r1 = $"{dts} * \"{s1}\" \"{s2}\""
      let am2s = function V (a, Commodity b) -> $"{a} {b}" | T _ -> "" | X _ -> ""
      let ac2s = function Account x -> x | VirtualisedAccount (a,b) -> $"{a}-{b}"
      let rs2 = [for (a,am,_) in e.Postings
                  do $"  {ac2s a|>safeAccount}    {am2s am}".TrimEnd()]
      [r1] @ rs2
    es |> List.collect g
  let entries = rs |> List.collect (fun (d,e) -> f d e)



  // build it and write it
  let content = header @ accounts @ entries
  File.WriteAllLines(outputfn, content)


let pp it ot ifn ofn =
  match it, ot with
    | Blossom, Beancount ->
        loadJournal ifn |> blossom2beancount ofn
    | _ -> failwith $"Conversion of {it} -> {ot} is unsupported at this time."


