module PrettyPrint

open System
open System.IO

open Journal
open FParsec
open Types
open Reports

type FileFormat = Blossom | Beancount | Ledger | Knut

// commodities
let safeCommodity (com : string) =
  let com1 = com.ToUpper().Replace(".", "").Replace("_", "")
  com1

let blossom2knut inputfn outputfn =
  let journal = loadJournal true DateTime.MaxValue inputfn
  match journal.Meta.Convention with
    | Some Financial5 | Some Financial7 -> true
    | _ -> failwith "Can only convert Financial5 or Financial7 journals"
    |> ignore

  // accounts
  let safeAccount (acc : string) =
    let acc1 = acc.Replace(" ", "-").Replace("/", "-").Replace("_", "")
    let acc2 = acc1.Replace("Payable:", "Liabilities:Payable:")
                   .Replace("Receivable:", "Assets:Receivable:")
                   .Replace("Liability:", "Liabilities:")
                   .Replace("Asset:", "Assets:")
                   .Replace("Expense:", "Expenses:")
                   .Replace("Market", "Assets:Market")
    acc2

  let date0 = journal.Register |> Map.toList |> List.map fst |> List.min |> fst
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
      let r1 = $"{dts} \"{s1} // {s2}\""
      let am2s = function (a, Commodity b) -> $"{a} {safeCommodity b}"
      let rs2 = [for (Account a, am, Account c) in e.Postings
                  do $"{safeAccount a}    {safeAccount c}   {am2s am}".TrimEnd()]
      [r1] @ rs2 @ [""]
    es |> List.collect g
  let entries = rs |> List.collect (fun (d,e) -> f (fst d) e)

  // prices
  let prices = journal.Prices |> Map.toList
                              |> List.collect (fun ((Commodity q1, Commodity q2), ps) -> ps |> Map.toList |> List.map (fun (dt,p) -> dt,q1,p,q2))
                              |> List.map (fun (dt,q1,p,q2) -> let dts = dt.ToString("yyyy-MM-dd")
                                                               $"{dts} price {safeCommodity q1} {fst p} {safeCommodity q2}")

  // build it and write it
  let content = accounts @ entries @ prices
  File.WriteAllLines(outputfn, content)

let blossom2beancount inputfn outputfn  =
  // preparation
  let journal = loadJournal true DateTime.MaxValue inputfn

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

  // accounts
  let safeAccount (acc : string) =
    let acc1 = acc.Replace(" ", "-").Replace("/", "-")
    let acc2 = acc1.Replace("Payable:", "Liability:").Replace("Receivable:", "Asset:")
    acc2

  let date0 = journal.Register |> Map.toList |> List.map fst |> List.min |> fst
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
      let am2s = function (a, Commodity b) -> $"{a} {safeCommodity b}"
      let rs2 = [for (Account a,am,_) in e.Postings
                  do $"  {safeAccount a}    {am2s am}".TrimEnd()]
      [r1] @ rs2
    es |> List.collect g
  let entries = rs |> List.collect (fun (d,e) -> f (fst d) e)

  // prices
  let prices = journal.Prices |> Map.toList
                              |> List.collect (fun ((Commodity q1, Commodity q2), ps) -> ps |> Map.toList |> List.map (fun (dt,p) -> dt,q1,p,q2))
                              |> List.map (fun (dt,q1,p,q2) -> let dts = dt.ToString("yyyy-MM-dd")
                                                               $"{dts} price X{safeCommodity q1} {fst p} {safeCommodity q2}")


  // build it and write it
  let content = header @ accounts @ entries @ prices
  File.WriteAllLines(outputfn, content)


let pp it ot ifn ofn =
  match it, ot with
    | Blossom, Beancount -> blossom2beancount ifn ofn
    | Blossom, Knut -> blossom2knut ifn ofn
    | _ -> failwith $"Conversion of {it} -> {ot} is unsupported at this time."


