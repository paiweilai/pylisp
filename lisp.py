############################
# LISP INTERPRETER PROJECT #
# DATE: WINTER 2010        #
# AUTHOR: PAI-WEI LAI      #
############################

import sys


def run():
    if len(sys.argv) != 2:
        print 'usage: $PYTHON %s [input_file]' % __file__
        return
    _, input_file = sys.argv

    print '********** LISP INTERPRETER INPUT **********'
    with open(input_file, 'r') as infile:
        source_text = infile.read()

    input_text = normalize_str(source_text)
    print '%r' % input_text

    print '********** LISP INTERPRETER OUTPUT *********'
    alist = {}
    dlist = {}
    sys.setrecursionlimit(10000)

    while len(input_text) != 0:
        token, remainder_str, token_type = get_next_token(input_text)
        if not check_parentheses(token):
            raise Exception('Parentheses mismatch: %r' % token)
        elif token_type == 'atom':
            if is_int(token) or token == 'T' or token == 'NIL':
                print '%r' % token  # output
        elif token_type == 'sexp':
            sexp = convert_str_to_sexp(remove_parentheses(token))
            print '%r' % convert_sexp_to_str(eval(sexp, alist, dlist), False)  # output
        else:
            raise Exception('Invalid s-expression: %r' % remainder_str)
        input_text = remainder_str
   

##################
# LISP FUNCTIONS #
##################

def eval(exp, alist, dlist):
    if atom(exp) == 'T':
        if is_int(exp):
            return exp
        elif eq(exp, ['T']) == 'T':
            return 'T'
        elif eq(exp, ['NIL']) == 'T':
            return 'NIL'
        elif exp[0] in alist:
            return alist[exp[0]]  # get value
        else:
            raise Exception('Unbound variable: %r' % exp)
    elif atom(car(exp)) == 'T':  # special cases
        if eq(car(exp), ['QUOTE']) == 'T':
            return car(cdr(exp))
        elif eq(car(exp), ['COND']) == 'T':
            return eval_cond(cdr(exp), alist, dlist)
        elif eq(car(exp), ['DEFUN']) == 'T':
            name = car(cdr(exp))[0]
            plist = car(cdr(cdr(exp)))
            body = car(cdr(cdr(cdr(exp))))
            dlist[name] = cons(plist, body)
            return name
        else:
            return apply(car(exp), eval_list(cdr(exp), alist, dlist), alist, dlist)
    raise Exception('eval invalid s-exp %r' % exp)


def eval_cond(be, alist, dlist):
    if null(be) == 'T':
        raise Exception('No more be for COND')
    elif eval(car(car(be)), alist, dlist) == 'T':
        return eval(car(cdr(car(be))), alist,dlist)
    else:
        return eval_cond(cdr(be), alist, dlist)


def eval_list(lst, alist, dlist):
    if null(lst) == 'T':
        return 'NIL'
    else:
        return cons(eval(car(lst), alist, dlist), eval_list(cdr(lst), alist, dlist))


def apply(f, x, alist, dlist):
    if not atom(f) == 'T':
        raise Exception('Apply %r', f)
    if eq(f, ['CAR']) == 'T':
        return car(car(x))
    elif eq(f, ['CDR']) == 'T':
        return cdr(car(x))
    elif eq(f, ['CONS']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return cons(car(x), car(cdr(x)))
    elif eq(f, ['ATOM']) == 'T':
        if length(x) != 1:
            raise Exception('Number of parameters mismatch')
        return atom(car(x))
    elif eq(f, ['EQ']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return eq(car(x), car(cdr(x)))
    elif eq(f, ['NULL']) == 'T':
        if length(x) != 1:
            raise Exception('Number of parameters mismatch')
        return null(car(x))
    elif eq(f, ['INT']) == 'T':
        if length(x) != 1:
            raise Exception('Number of parameters mismatch')
        return is_int(car(x))
    elif eq(f, ['PLUS']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return plus(car(x), car(cdr(x)))
    elif eq(f, ['MINUS']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return minus(car(x), car(cdr(x)))
    elif eq(f, ['TIMES']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return times(car(x), car(cdr(x)))
    elif eq(f, ['QUOTIENT']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return quotient(car(x), car(cdr(x)))
    elif eq(f, ['REMAINDER']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return remainder(car(x), car(cdr(x)))
    elif eq(f, ['LESS']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return less(car(x), car(cdr(x)))
    elif eq(f, ['GREATER']) == 'T':
        if length(x) != 2:
            raise Exception('Number of parameters mismatch')
        return greater(car(x), car(cdr(x)))
    else:  # search d-list for user defined functions
        if not dlist.has_key(f[0]):
            raise Exception('Function %r not define' % f[0])
        plist, body = dlist[f[0]]
        if length(plist) != length(x):
            raise Exception('Number of parameters mismatch')
        if length(plist) == 0:
            updated_alist = alist  # special case: (DEFUN X () fb)
        else:
            updated_alist = add_pair(plist, x, alist)
        return eval(body, updated_alist, dlist)


def add_pair(plist, x, alist):
    """Add a pair to a-list."""
    new_list = alist.copy()
    while True:
        key = car(plist)[0]
        val = car(x)[0]
        new_list[key] = val
        if cdr(plist) == ['NIL']:
            break
        else:
            plist = cdr(plist)
            x = cdr(x)
    return new_list


####################
# HELPER FUNCTIONS #
####################

def normalize_str(string):
    string = string.upper()
    string = string.replace('\n', ' ')
    string = string.replace('\r', ' ')
    string = string.replace('()', 'NIL')
    while '  ' in string:
        string = string.replace('  ', ' ')
    string = string.replace('+', '')
    string = string.replace(' . ', '.')
    string = string.replace('. ', '.')
    string = string.replace(' .', '.')
    string = string.replace(' )', ')')
    string = string.replace('( ', '(')
    string = string.lstrip()
    string = string.rstrip()
    return string


def convert_str_to_sexp(string):
    """Convert a string into a s-exp binary tree."""
    tokens = []
    while len(string) != 0:
        token, remainder_str, token_type = get_next_token(string)
        if token_type == 'sexp':  # recursively convert it into a binary tree
            token = remove_parentheses(token)
            token = convert_str_to_sexp(token)
        string = remainder_str
        tokens.append(token)
    if tokens[0] == '.':  # example: (. 1 2 3)
        raise Exception('Invalid s-exp, the first element cannot be a dot: %r' % tokens)
    num = 0;
    for x in range(0, len(tokens)):  # compute the number of dots
        if tokens[x] == '.':
            num += 1
    if num > 1:  # example: (1 . 2 . 3)
        raise Exception('Invalid dot notation, there should be only one dot: %r' % tokens)
    if len(tokens) == 1:
        return [tokens[0], 'NIL']  # example: (1)
    if tokens[1] == '.':
        return convert_dot_to_tree(tokens)  # dot notation
    if num > 0: # example: (1 2 . 3)
        raise Exception('Invalid list notation, there should not be any dot: %r' % tokens)
    return convert_list_to_tree(tokens)  # list notation


def convert_sexp_to_str(sexp, is_top_level):
    """Convert a s-exp into a string."""
    if not isinstance(sexp, list):
        if is_top_level:
            return ')'  # end of a list notation
        else:
            return sexp  # end of a dot notation
    if len(sexp) == 1:
        return sexp[0]
    left, right = sexp
    if is_list(sexp):  # check for dot or list notation
        if is_top_level:
            # middle level of a list
            result = convert_sexp_to_str(left, False) + ' ' + convert_sexp_to_str(right, True)
        else:
            # top level begins with '('
            result = '(' + convert_sexp_to_str(left, False) + ' ' + convert_sexp_to_str(right, True)
    else:
        # dot notation
        result = '(' + convert_sexp_to_str(left, False) + ' . ' + convert_sexp_to_str(right, False) + ')'
    result = result.replace(' )', ')')
    return result


def is_list(sexp):
    """Check if a s-exp is a list or not."""
    if not isinstance(sexp, list):
        return False
    if len(sexp) == 1:
        return False
    while isinstance(sexp, list) and len(sexp) > 1:
        sexp = sexp[1]
    if sexp == 'NIL':
        return True
    return False


def length(sexp):
    """Return the length of a s-exp."""
    num = 0
    while isinstance(sexp, list) and len(sexp) > 1:
        sexp = sexp[1]
        num += 1
    return num


def convert_dot_to_tree(tokens):
    """Convert a dot notation of tokens into a binary tree."""
    tokens.pop(1)
    if len(tokens) != 2:  # there should be exact three tokens in a dot notation: [e1,'.',e2]
        raise Exception('Invalid dot notation, there should be only two elements: %r' % tokens)
    tree = [tokens[0], tokens[1]]
    return tree


def convert_list_to_tree(tokens):
    """Convert a list of tokens into a binary tree."""
    if len(tokens) == 1:
        return [tokens[0], 'NIL']
    return [tokens[0], convert_list_to_tree(tokens[1:])]


def get_next_token(string):
    string = string.lstrip()
    string = string.rstrip()
    if string.startswith('('):  # s-expression token
        n = 0
        for x in range(0, len(string)):
            if string[x] == '(':
                n = n + 1
            elif string[x] == ')':
                n = n - 1
            if n == 0:
                # find corresponding ')' and return this s-exp string
                return [string[0:x+1], string[x+1:], 'sexp']
        if n > 0:
            return [[], string, 'error']  # parentheses does not match
    if string.startswith('.'):
        return ['.', string[1:], 'dot']  # dot token
    for x in range(0, len(string)):  # atom token
        if string[x] == ' ' or string[x] == '.' or string[x] == '(':
            return [string[0:x], string[x:], 'atom']
    return [string, [], 'atom']
    

def remove_parentheses(string):
    string = string.lstrip()
    string = string.rstrip()
    if string.startswith('(') and string.endswith(')'):
        string = string[1:-1]  # remove '(' and ')'
    return string


def check_parentheses(string):
    """Check if the numbers of '(' and ')' match."""
    n = 0
    for x in range(0, len(string)):
        if string[x] == '(':
            n = n + 1
        elif string[x]==')':
            n = n - 1
        if n < 0:
            return False
    if n != 0:
        return False
    return True


###################
# LISP PRIMITIVES #
###################

def car(s):
    if atom(s) == 'T':
        raise Exception('car an atomic sexp: %r' % s)
    if not isinstance(s[0], list):
        return [s[0]]
    return s[0]


def cdr(s):
    if atom(s) == 'T':
        raise Exception('cdr an atomic sexp: %r' % s)
    if not isinstance(s[1], list):
        return [s[1]]
    return s[1]


def cons(s1, s2):
    return [s1, s2]


def atom(s):
    if len(s) == 1:
        return 'T'
    return 'NIL'


def eq(s1, s2):
    if not (atom(s1) == 'T' and atom(s2) == 'T'):
        raise Exception('non-atom pair: (%r, %r)' % (s1, s2))
    if s1 == s2:
        return 'T'
    return 'NIL'


def null(s):
    if atom(s) == 'T' and eq(s, ['NIL']) == 'T':
        return 'T'
    return 'NIL'


def is_int(s):
    """Check if s is a integer number."""
    if s[0][0] == '-':
        tmp = s[0][1:]
    else:
        tmp = s[0][:]
    if isinstance(tmp, list):
        return 'NIL'
    return tmp.isdigit()


def check_input_sexps(s1, s2):
    """Raise exception if s1 and s2 are not both atom or both int."""
    if not (atom(s1) == 'T' and atom(s2) == 'T'):
        raise Exception('non-atom pair: (%r, %r)' % (s1, s2))
    if not (is_int(s1) and is_int(s2)):
        raise Exception('non-integer pair: (%r, %r)' % (s1, s2))

def plus(s1, s2):
    """s1 + s2"""
    check_input_sexps(s1, s2)
    return str(int(s1[0]) + int(s2[0]))


def minus(s1, s2):
    """s1 - s2"""
    check_input_sexps(s1, s2)
    return str(int(s1[0]) - int(s2[0]))


def times(s1, s2):
    """s1 * s2"""
    check_input_sexps(s1, s2)
    return str(int(s1[0]) * int(s2[0]))


def quotient(s1, s2):
    """s1 / s2"""
    check_input_sexps(s1, s2)
    return str(int(s1[0]) / int(s2[0]))


def remainder(s1, s2):
    """s1 % s2"""
    check_input_sexps(s1, s2)
    return str(int(s1[0]) % int(s2[0]))


def less(s1, s2):
    """Return True if s1 < s2."""
    check_input_sexps(s1, s2)
    if int(s1[0]) < int(s2[0]):
        return 'T'
    return 'NIL'


def greater(s1, s2):
    """Return True if s1 > s2."""
    check_input_sexps(s1, s2)
    if int(s1[0]) > int(s2[0]):
        return 'T'
    return 'NIL'


if __name__ == '__main__':
    run()
