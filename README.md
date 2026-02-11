# Lambda Calculus Interpreter
---

## 📌 Descripción

Este proyecto consiste en la implementación de un **intérprete de cálculo lambda tipado**.  
El objetivo es construir un pequeño lenguaje funcional, junto con su **sistema de tipos** y un **evaluador**, siguiendo los principios formales del λ-cálculo y los lenguajes funcionales.

El intérprete está desarrollado en **OCaml** e incluye una interfaz interactiva tipo **REPL** (*Read–Eval–Print Loop*).

---

## 🚀 Funcionamiento básico

El punto de entrada del programa es un **REPL**, desde el cual el usuario puede:

- Evaluar expresiones del lenguaje.
- Definir términos globales.
- Definir alias de tipos.
- Usar comandos básicos como salir o limpiar la pantalla.

Las expresiones se terminan siempre con `;;`, lo que permite escribir expresiones en varias líneas.

Ejemplo:

```text
>> lambda x:{Nat,Nat}.x.1;;
- : {Nat * Nat} -> Nat = (lambda x:{Nat * Nat}.x.1)
