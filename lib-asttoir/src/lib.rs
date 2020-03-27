use ir;

/**
 * The structs functions serve as utility functions to extract information from the AST
 * into a suitable Intermediate Representation.
 * In particular:
 * * Entry points are zero-indexed. Main function will be #0
 * * For now, we will leave everything as type Any
 * Sample function that we will be decoding
 *
* *
* *
 */

pub fn read_from_file(filename: Option<std::string::String>) -> serde_json::Value {
    let default_filename = "./ast.txt".to_string();
    let contents = std::fs::read_to_string(filename.unwrap_or(default_filename))
        .expect("Something went wrong reading the file");
    let json_contents: serde_json::Value = serde_json::from_str(&contents).unwrap();
    return json_contents;
}

pub fn populate_funcs() {
    todo!();
}

pub fn populate_func(ast: serde_json::Value) -> ir::Func {
    let func: ir::Func = ir::Func {
        params: populate_func_params(ast.clone()),
        result: populate_func_result(ast.clone()),
        locals: populate_func_locals(ast.clone()),
        statements: populate_func_statements(ast.clone()),
        signature_filter: vec![],
    };
    return func;
}

pub fn populate_func_params(ast: serde_json::Value) -> Box<[ir::VarType]> {
    let mut func_params = Vec::<ir::VarType>::new();
    let prog_body = ast["body"][0].clone();
    if prog_body["type"] == "FunctionDeclaration" {
        for i in prog_body["params"].as_array().unwrap() {
            func_params.push(ir::VarType::Any);
        }
    }
    return func_params.into_boxed_slice();
}

pub fn populate_func_result(ast: serde_json::Value) -> Option<ir::VarType> {
    return Some(ir::VarType::Any);
}

pub fn populate_func_locals(ast: serde_json::Value) -> Vec<ir::VarType> {
    let mut func_locals = Vec::<ir::VarType>::new();
    let prog_body = ast["body"][0].clone();
    let prog_locals = prog_body["body"]["body"][1]["declarations"].clone();

    for j in 0..prog_body["body"]["body"].as_array().unwrap().len() {
        if prog_body["body"]["body"][j]["type"] == "VariableDeclaration" {
            func_locals.push(ir::VarType::Any);
        }
    }

    return func_locals;
}

pub fn populate_func_statements(ast: serde_json::Value) -> ir::Block {
    // Doesn't support nested functions
    // Statements are either Assign, Return, If, Expr, Void
    // type Expression = "ThisExpression" || "Identifier" || "Literal" ||
    // "ArrayExpression" || "ObjectExpression" || "FunctionExpression" || "ArrowFunctionExpression" || "ClassExpression" ||
    // "TaggedTemplateExpression" || "MemberExpression" || "Super" || "MetaProperty" ||
    // "NewExpression" || "CallExpression" || "UpdateExpression" || "AwaitExpression" || "UnaryExpression" ||
    // "BinaryExpression" || "LogicalExpression" || "ConditionalExpression" ||
    // "YieldExpression"  || "SequenceExpression";
    // If it's a function we loop through body and then classify each of the statements
    // If      -> Conditional Expression
    // Assign  -> AssignmentExpression
    // Return  -> Return Expression
    // Loop through each statment in body
    let mut func_statements = Vec::<ir::Statement>::new();
    let mut index = 0;

    for i in 0..ast["body"][0]["body"]["body"].as_array().unwrap().len() {
        let statement_type = ast["body"][0]["body"]["body"][i]["type"].clone();
        println!("{}", ast["body"][0]["body"]["body"][i]["type"]);
        if statement_type == "ConditionalExpression" {
            // let conditional_expression = ir::Statement::If {
            //     cond: Expr,
            //     true_stmts: Block,
            //     false_stmts: Block,
            // }
            // Create condtional expression
            //     func_statements.push(conditional_expression)
        } else if statement_type == "AssignmentExpression" {
            let assignment_statement = ir::Statement::Assign {
                target: ir::TargetExpr::Local {
                    localidx: 0,
                    // Figure out how to initialize a new struct field
                    next: None,
                },
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    kind: ir::ExprKind::PrimUndefined,
                },
            };
            // Create Assignment  Statement
            func_statements.push(assignment_statement);
        } else if statement_type == "ReturnStatement" {
            let return_statement = ir::Statement::Return {
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: index,
                            next: None,
                        },
                    },
                },
            };
            func_statements.push(return_statement);
            index += 1;
        } else if statement_type == "VariableDeclaration" {
            // [TODO] (Joel): Modify this to handle all types of expressions
            let expression_statement = ir::Statement::Expr {
                expr: ir::Expr {
                    vartype: ir::VarType::Any,
                    // [TODO] (Joel): Create a function to generate TargetExpr
                    kind: ir::ExprKind::VarName {
                        source: ir::TargetExpr::Local {
                            localidx: index,
                            next: None,
                        },
                    },
                },
            };
            func_statements.push(expression_statement);
            index += 1;
        } else {
        }
    }
    return func_statements;
}

// Test Case:
// ```
// function f() {
// const x = 4;
// const y = x * x;
// const z = y + 5;
// return z;
// }
// ```

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_can_populate_locals() {
        let ast = read_from_file(None);
        let actual_locals = populate_func_locals(ast);
        let expected_locals = vec![ir::VarType::Any, ir::VarType::Any, ir::VarType::Any];
        assert_eq!(expected_locals, actual_locals);
    }

    #[test]
    fn it_can_populate_result() {
        let ast = read_from_file(None);
        let actual_result = populate_func_result(ast);
        let expected_result = Some(ir::VarType::Any);
        assert_eq!(actual_result, expected_result);
    }

    #[test]
    fn it_can_populate_func_params() {
        let ast = read_from_file(None);
        let actual_params = populate_func_params(ast);
        let expected_params = Box::new([ir::VarType::Any, ir::VarType::Any]);
        assert_eq!(*actual_params, *expected_params);
    }

    // #[test]
    // fn it_can_populate_func_statements() {
    //     let ast = read_from_file(None);
    //     let actual_statements = populate_func_statements(ast);
    //     // let expected_statements = vec![ir::Block;ir::Exp];
    //     // assert_eq!(*actual_params, *expected_params);
    //     todo!();
    // }
}
