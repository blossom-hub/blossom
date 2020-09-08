module Journal

open JournalParser


let loadJournal imports filename =
  let rjournal = loadRJournal filename

  rjournal