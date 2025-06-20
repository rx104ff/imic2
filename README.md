# Derivation Engine for Mini-ML

This is a **derivation engine** for a simplified ML-style language, built to generate evaluation derivation trees using small-step or big-step operational semantics.

## Usage

```bash
cargo run -- <RULE_NAME> "PROGRAM"
```

* `<RULE_NAME>` must be one of the supported derivation rule sets.
* `"PROGRAM"` is a quoted ML-style expression.

## Supported Rules

Currently, the following derivation rule sets are implemented:

1. **ML1** — Integer and Boolean expressions
2. **ML2** — List and `match` evaluation
3. **ML3** — Function definitions, applications, and closures
4. **ML4** — Full support with let-bindings, recursion, and environments

## Example

### ML1
```text
 3 + 5 evalto 8 by E-Plus {
     3 evalto 3 by E-Int {
    };
     5 evalto 5 by E-Int {
    };
     3 plus 5 is 8 by B-Plus {};
};
```

### ML2
```text
|- 1 + 2 * 3 evalto 7 by E-Plus {
    |- 1 evalto 1 by E-Int {};
    |- 2 * 3 evalto 6 by E-Times {
        |- 2 evalto 2 by E-Int {};
        |- 3 evalto 3 by E-Int {};
        2 times 3 is 6 by B-Times {};
    };
    1 plus 6 is 7 by B-Plus {};
};

```
### ML3
```text
|- let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2 evalto 65536 by E-Let {
     |- fun f -> fun x -> f (f x) evalto ()[fun f -> fun x -> f (f x)] by E-Fun {
    };
     twice = ()[fun f -> fun x -> f (f x)] |- twice twice (fun x -> x * x) 2 evalto 65536 by E-App {
         twice = ()[fun f -> fun x -> f (f x)] |- twice twice (fun x -> x * x) evalto (f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)])[fun x -> f (f x)] by E-App {
             twice = ()[fun f -> fun x -> f (f x)] |- twice twice evalto (f = ()[fun f -> fun x -> f (f x)])[fun x -> f (f x)] by E-App {
                 twice = ()[fun f -> fun x -> f (f x)] |- twice evalto ()[fun f -> fun x -> f (f x)] by E-Var1 {
                };
                 twice = ()[fun f -> fun x -> f (f x)] |- twice evalto ()[fun f -> fun x -> f (f x)] by E-Var1 {
                };
                 f = ()[fun f -> fun x -> f (f x)] |- fun x -> f (f x) evalto (f = ()[fun f -> fun x -> f (f x)])[fun x -> f (f x)] by E-Fun {
                };
            };
             twice = ()[fun f -> fun x -> f (f x)] |- (fun x -> x * x) evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Fun {
            };
             f = ()[fun f -> fun x -> f (f x)], x = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f (f x) evalto (f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)])[fun x -> f (f x)] by E-App {
                 f = ()[fun f -> fun x -> f (f x)], x = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ()[fun f -> fun x -> f (f x)] by E-Var2 {
                     f = ()[fun f -> fun x -> f (f x)] |- f evalto ()[fun f -> fun x -> f (f x)] by E-Var1 {
                    };
                };
                 f = ()[fun f -> fun x -> f (f x)], x = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- (f x) evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-App {
                     f = ()[fun f -> fun x -> f (f x)], x = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ()[fun f -> fun x -> f (f x)] by E-Var2 {
                         f = ()[fun f -> fun x -> f (f x)] |- f evalto ()[fun f -> fun x -> f (f x)] by E-Var1 {
                        };
                    };
                     f = ()[fun f -> fun x -> f (f x)], x = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- x evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var1 {
                    };
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- fun x -> f (f x) evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-Fun {
                    };
                };
                 f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] |- fun x -> f (f x) evalto (f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)])[fun x -> f (f x)] by E-Fun {
                };
            };
        };
         twice = ()[fun f -> fun x -> f (f x)] |- 2 evalto 2 by E-Int {
        };
         f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)], x = 2 |- f (f x) evalto 65536 by E-App {
             f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)], x = 2 |- f evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-Var2 {
                 f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] |- f evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-Var1 {
                };
            };
             f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)], x = 2 |- (f x) evalto 16 by E-App {
                 f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)], x = 2 |- f evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-Var2 {
                     f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] |- f evalto (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)] by E-Var1 {
                    };
                };
                 f = (f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]))[fun x -> f (f x)], x = 2 |- x evalto 2 by E-Var1 {
                };
                 f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 2 |- f (f x) evalto 16 by E-App {
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 2 |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var2 {
                         f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var1 {
                        };
                    };
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 2 |- (f x) evalto 4 by E-App {
                         f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 2 |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var2 {
                             f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var1 {
                            };
                        };
                         f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 2 |- x evalto 2 by E-Var1 {
                        };
                         twice = ()[fun f -> fun x -> f (f x)], x = 2 |- x * x evalto 4 by E-Times {
                             twice = ()[fun f -> fun x -> f (f x)], x = 2 |- x evalto 2 by E-Var1 {
                            };
                             twice = ()[fun f -> fun x -> f (f x)], x = 2 |- x evalto 2 by E-Var1 {
                            };
                             2 times 2 is 4 by B-Times {};
                        };
                    };
                     twice = ()[fun f -> fun x -> f (f x)], x = 4 |- x * x evalto 16 by E-Times {
                         twice = ()[fun f -> fun x -> f (f x)], x = 4 |- x evalto 4 by E-Var1 {
                        };
                         twice = ()[fun f -> fun x -> f (f x)], x = 4 |- x evalto 4 by E-Var1 {
                        };
                         4 times 4 is 16 by B-Times {};
                    };
                };
            };
             f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 16 |- f (f x) evalto 65536 by E-App {
                 f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 16 |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var2 {
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var1 {
                    };
                };
                 f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 16 |- (f x) evalto 256 by E-App {
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 16 |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var2 {
                         f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) |- f evalto ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]) by E-Var1 {
                        };
                    };
                     f = ((twice = ()[fun f -> fun x -> f (f x)])[fun x -> x * x]), x = 16 |- x evalto 16 by E-Var1 {
                    };
                     twice = ()[fun f -> fun x -> f (f x)], x = 16 |- x * x evalto 256 by E-Times {
                         twice = ()[fun f -> fun x -> f (f x)], x = 16 |- x evalto 16 by E-Var1 {
                        };
                         twice = ()[fun f -> fun x -> f (f x)], x = 16 |- x evalto 16 by E-Var1 {
                        };
                         16 times 16 is 256 by B-Times {};
                    };
                };
                 twice = ()[fun f -> fun x -> f (f x)], x = 256 |- x * x evalto 65536 by E-Times {
                     twice = ()[fun f -> fun x -> f (f x)], x = 256 |- x evalto 256 by E-Var1 {
                    };
                     twice = ()[fun f -> fun x -> f (f x)], x = 256 |- x evalto 256 by E-Var1 {
                    };
                     256 times 256 is 65536 by B-Times {};
                };
            };
        };
    };
};
```
### ML4
```text
|- let f = fun x -> match x with [] -> 0 | a :: b -> a in f (4 :: []) + f [] + f (1 :: 2 :: 3 :: []) evalto 5 by E-Let {
     |- fun x -> match x with [] -> 0 | a :: b -> a evalto ()[fun x -> match x with [] -> 0 | a :: b -> a] by E-Fun {
    };
     f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f (4 :: []) + f [] + f (1 :: 2 :: 3 :: []) evalto 5 by E-Plus {
         f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f (4 :: []) + f [] evalto 4 by E-Plus {
             f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f (4 :: []) evalto 4 by E-App {
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f evalto ()[fun x -> match x with [] -> 0 | a :: b -> a] by E-Var {
                };
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- (4 :: []) evalto (4 :: []) by E-Cons {
                     f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 4 evalto 4 by E-Int {
                    };
                     f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- [] evalto [] by E-Nil {
                    };
                };
                 x = (4 :: []) |- match x with [] -> 0 | a :: b -> a evalto 4 by E-MatchCons {
                     x = (4 :: []) |- x evalto (4 :: []) by E-Var {
                    };
                     x = (4 :: []), a = 4, b = [] |- a evalto 4 by E-Var {
                    };
                };
            };
             f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f [] evalto 0 by E-App {
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f evalto ()[fun x -> match x with [] -> 0 | a :: b -> a] by E-Var {
                };
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- [] evalto [] by E-Nil {
                };
                 x = [] |- match x with [] -> 0 | a :: b -> a evalto 0 by E-MatchNil {
                     x = [] |- x evalto [] by E-Var {
                    };
                     x = [] |- 0 evalto 0 by E-Int {
                    };
                };
            };
             4 plus 0 is 4 by B-Plus {};
        };
         f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f (1 :: 2 :: 3 :: []) evalto 1 by E-App {
             f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- f evalto ()[fun x -> match x with [] -> 0 | a :: b -> a] by E-Var {
            };
             f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- (1 :: 2 :: 3 :: []) evalto (1 :: 2 :: 3 :: []) by E-Cons {
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 1 evalto 1 by E-Int {
                };
                 f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 2 :: 3 :: [] evalto 2 :: 3 :: [] by E-Cons {
                     f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 2 evalto 2 by E-Int {
                    };
                     f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 3 :: [] evalto 3 :: [] by E-Cons {
                         f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- 3 evalto 3 by E-Int {
                        };
                         f = ()[fun x -> match x with [] -> 0 | a :: b -> a] |- [] evalto [] by E-Nil {
                        };
                    };
                };
            };
             x = (1 :: 2 :: 3 :: []) |- match x with [] -> 0 | a :: b -> a evalto 1 by E-MatchCons {
                 x = (1 :: 2 :: 3 :: []) |- x evalto (1 :: 2 :: 3 :: []) by E-Var {
                };
                 x = (1 :: 2 :: 3 :: []), a = 1, b = 2 :: 3 :: [] |- a evalto 1 by E-Var {
                };
            };
        };
         4 plus 1 is 5 by B-Plus {};
    };
};


```


## 📜 License

MIT License.
