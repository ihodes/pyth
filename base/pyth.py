def tokenize(string):
    # reversed because we want to consume a pre-order stack of tokens
    return string.replace('(', ' ( ').replace(')', ' ) ').split()[::-1]


def read_expression(tokens):
    expression = []
    token = tokens.pop()
    if token == '(':
        while True:
            token = tokens.pop()
            if token is ')':
                return expression
            elif token is '(':
                tokens.append(token)
                expression.append(read_expression(tokens))
            else:
                expression.append(token)
    else:
        return token


def read_expressions(tokens):
    expressions = []
    while tokens:
        expressions.append(read_expression(tokens))
    return expressions


def read(string):
    return read_expressions(tokenize(string))


class Env(dict):
    def __init__(self, vars=(), vals=(), env=None):
        self.update(zip(vars, vals))
        self.env = env
    def lookup(self, var):
        if var in self:
            return self[var]
        elif self.env:
            return self.env.lookup(var)
        else:
            raise ValueError('"' + var + '" not in scope.')


FUNCTIONS = {'atom': lambda x: not isinstance(x, list), 'eq': lambda x, y: x == y,
             'cons': lambda x, y: [x] + y if y else [x], 'car': lambda x: x[0] if x else None,
             'cdr': lambda x: x[1:]}

SENTINELS = {'nil': None}


def eval(expr, env):
    if isinstance(expr, str):
        if expr in SENTINELS:
            return SENTINELS[expr]
        return env.lookup(expr)
    elif expr[0] == 'quote':
        return expr[1]
    elif expr[0] == 'cond':
        for clause in expr[1:]:
            if clause[0] == 'else' or eval(clause[0], env):
                return [eval(expr, env) for expr in clause[1:]][-1]
    elif expr[0] == 'lambda':
        vars, subexprs = expr[1], expr[2:]
        return lambda *args: [eval(e, Env(vars=vars, vals=args, env=env))
                              for e in subexprs][-1]
    elif expr[0] == 'label':
        _, var, expr = expr
        env[var] = eval(expr, env)
        return (var, eval(expr, env))
    else:
        expr = [eval(e, env) for e in expr]
        return expr[0](*expr[1:])


def pr(expr):
    if expr in SENTINELS.values():
        return dict([v, k] for k, v in SENTINELS.items())[expr]
    elif isinstance(expr, list):
        return '(' + ' '.join(str(pr(e)) for e in expr) + ')'
    else:
        return expr


def loop(env, prompt='> '):
    while True:
        try:
            string = raw_input(prompt)
            if not string:
                print
            elif string[0] == '%' and string[1:5] == 'load':
                with open(string[6:] + '.scm') as f:
                    string = '\n'.join(line for line in f.readlines()
                                       if line and line[0] is not ';')
            else:
                while string.count('(') != string.count(')'):
                    string = string + raw_input()
            for val in (eval(expr, env) for expr in read(string)):
                print(pr(val))
        except EOFError:
            print "\nExiting REPL..."
            break
        except Exception as e:
            print "Error: ", e


GLOBAL = Env()
GLOBAL.update(FUNCTIONS)
if __name__ == '__main__':
    loop(global_env, '> ')


assert (tokenize('(apply max (cons (+ 1 2) somelist))') == [')', ')', 'somelist', ')', '2', '1', '+', '(', 'cons', '(', 'max', 'apply', '('])
assert (read_expression(['(', 'apply', 'max', '(', 'cons', '(', '+', '1', '2', ')', 'somelist', ')', ')'][::-1]) == ['apply', 'max', ['cons', ['+', '1', '2'], 'somelist']])
assert pr(['cons', '1', ['+', '1', '2']]) == '(cons 1 (+ 1 2))'
assert eval(['cons', ['quote', 'apple'], 'nil'], GLOBAL) == ['apple']
