module BankAccount

let _lock = System.Object()

type Account = {
    mutable isOpen: bool
    mutable balance: decimal
}

let mkBankAccount() =
    { isOpen = false; balance = 0m }

let openAccount account =
    account.isOpen <- true
    account

let closeAccount account =
    account.isOpen <- false
    account

let getBalance account =
    if account.isOpen then Some account.balance else None

let updateBalance change account =
    lock _lock (fun () -> account.balance <- account.balance + change) |> ignore
    account