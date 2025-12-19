use crate::compiler::{Scope, WriteRuby};
use crate::expr::Expr;

pub fn require_str(expr: &Expr) -> String {
    let Expr::String(expr_string) = expr else {
        panic!("Compiler intrinsic requires expression to be a string.");
    };

    expr_string.to_string()
}

pub fn static_method_call(scope: &mut Scope, receiver: &str, name: &str, positional_args: &[Expr]) {
    scope.fragment(format!("{receiver}.{name}("));

    for (index, positional_arg) in positional_args.iter().enumerate() {
        positional_arg.write_ruby(scope);

        if index < positional_args.len() - 1 {
            scope.fragment(", ");
        }
    }

    scope.fragment(")");
}

pub fn nullable_static_method_call(
    scope: &mut Scope,
    receiver: &str,
    name: &str,
    positional_args: &[Expr],
) {
    // TODO: Generate unique name for temporary result.
    scope.fragment("__oxiby_temp = ");

    static_method_call(scope, receiver, name, positional_args);

    scope.newline();

    scope.fragment(
        "__oxiby_temp.nil? ? ::Std::Option::Option::None.new : \
         ::Std::Option::Option::Some.new(__oxiby_temp)",
    );
}

pub fn instance_method_call(
    scope: &mut Scope,
    receiver: &Expr,
    name: &str,
    positional_args: &[Expr],
) {
    receiver.write_ruby(scope);
    scope.fragment(format!(".{name}("));

    for (index, positional_arg) in positional_args.iter().enumerate() {
        positional_arg.write_ruby(scope);

        if index < positional_args.len() - 1 {
            scope.fragment(", ");
        }
    }

    scope.fragment(")");
}

pub fn nullable_instance_method_call(
    scope: &mut Scope,
    receiver: &Expr,
    name: &str,
    positional_args: &[Expr],
) {
    // TODO: Generate unique name for temporary result.
    scope.fragment("__oxiby_temp = ");

    instance_method_call(scope, receiver, name, positional_args);

    scope.newline();

    scope.fragment(
        "__oxiby_temp.nil? ? ::Std::Option::Option::None.new : \
         ::Std::Option::Option::Some.new(__oxiby_temp)",
    );
}

pub fn wrap(scope: &mut Scope, block: &Expr) {
    scope.fragment("::Std::Ruby.wrap(");
    block.write_ruby(scope);
    scope.fragment(")");
}
