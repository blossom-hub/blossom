module PrettyPrint

open System
open System.IO

open Journal
open FParsec
open Types
open Reports

type FileFormat = Blossom | Beancount | Ledger | Knut

let blossom2beancount inputfn outputfn  =
  // preparation
  let journal = loadJournal inputfn

  // validation
  match journal.Meta.Convention with
    | Some Financial5 | Some Financial7 -> true
    | _ -> failwith "Can only convert Financial5 or Financial7 journals"
    |> ignore

  // header
  let oc = match journal.Meta.Commodity with | Some (Commodity c) -> c | _ -> "USD"
  let header = [
    $"option \"title\" \"{journal.Meta.Name}\""
    $"option \"operating_currency\" \"{oc}\""
    $"option \"render_commas\" \"TRUE\""
    $"option \"name_assets\" \"Asset\""
    $"option \"name_expenses\" \"Expense\""
    $"option \"name_liabilities\" \"Liability\""
    $"option \"name_expenses\" \"Expense\""
    $"option \"name_income\" \"Income\""
  ]

  // commodities
  let safeCommodity (com : string) =
    let com1 = com.ToUpper().Replace(".", "")
    com1

  // accounts
  let safeAccount (acc : string) =
    let acc1 = acc.Replace(" ", "-").Replace("/", "-")
    let acc2 = acc1.Replace("Payable:", "Liability:").Replace("Receivable:", "Asset:")
    acc2

  let date0 = journal.Register |> Map.toList |> List.map fst |> List.min
  let metaRequest = {RequestType = Accounts; Regex = None}
  let (MetaResultSet accountList) = meta id metaRequest journal
  let accounts = [for account in (accountList |> Set.filter (fun x -> not(x.Contains("/"))))
                    do $"""{date0.ToString("yyyy-MM-dd")} open {safeAccount account}"""]

  // entries
  let rs = journal.Register |> Map.toList
  let f (dt: DateTime) es =
    let dts = dt.ToString("yyyy-MM-dd")
    let g (e : Entry) =
      let s1 = match e.Payee with | Some x -> x | None -> e.Narrative
      let s2 = match e.Payee with | Some x -> e.Narrative | None -> ""
      let r1 = $"{dts} * \"{s1}\" \"{s2}\""
      let am2s = function | V (a, Commodity b) -> $"{a} {safeCommodity b}"
                          | T ((q1, Commodity c1), (q2, Commodity c2), _) -> $"{q1} X{safeCommodity c1} {{{q2} {safeCommodity c2}}}"
                          | X _ -> ""
      let ac2s = function Account x -> x | VirtualisedAccount (a, _) -> a
      let rs2 = [for (a,am,_) in e.Postings
                  do $"  {ac2s a |> safeAccount}    {am2s am}".TrimEnd()]
      [r1] @ rs2
    es |> List.collect g
  let entries = rs |> List.collect (fun (d,e) -> f d e)

  // prices
  let prices = journal.Prices |> Map.toList
                              |> List.collect (fun ((Commodity q1, Commodity q2), ps) -> ps |> Map.toList |> List.map (fun (dt,p) -> dt,q1,p,q2))
                              |> List.map (fun (dt,q1,p,q2) -> let dts = dt.ToString("yyyy-MM-dd")
                                                               $"{dts} price X{safeCommodity q1} {p} {safeCommodity q2}")


  // build it and write it
  let content = header @ accounts @ entries @ prices
  File.WriteAllLines(outputfn, content)


let pp it ot ifn ofn =
  match it, ot with
    | Blossom, Beancount -> blossom2beancount ifn ofn
    | _ -> failwith $"Conversion of {it} -> {ot} is unsupported at this time."


