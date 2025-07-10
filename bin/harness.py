import os
import re
import random
import argparse
import subprocess


class NatType():
    def name(self, ocaml=False):
        if ocaml:
            return 'tNat'

        return 'Nat'


class FloatType():
    def name(self, ocaml=False):
        if ocaml:
            return 'tFloat'

        return 'Float'


class StringType():
    def name(self, ocaml=False):
        if ocaml:
            return 'tString'

        return 'String'


class BoolType():
    def name(self, ocaml=False):
        if ocaml:
            return 'tBool'

        return 'Bool'


class VecType():
    global TYPES

    def __init__(self):
        self.size = random.randint(1, 100)
        i = random.randint(0, len(TYPES) - 1)
        self.polymorphic_type = TYPES[i]()

    def name(self, ocaml=False):
        if ocaml:
            return f"tVec ({self.polymorphic_type.name(True)}) {self.size}"

        lb = ""
        rb = ""
        if type(self.polymorphic_type) == VecType:
            lb = "("
            rb = ")"

        return f"Vector {lb}{self.polymorphic_type.name()}{rb} {self.size}"


class FuncType():
    global TYPES

    def __init__(self):
        self.arguments = []
        self.return_type = None

        size = random.randint(1, 4)

        for _ in range(size):
            i = random.randint(0, len(TYPES) - 1)
            self.arguments.append(TYPES[i]())

        # refrain from returning a function type
        i = random.randint(0, len(TYPES) - 1)
        self.return_type = TYPES[i]()

    def name(self, ocaml=False):
        if ocaml:
            s = "(["

            for arg in self.arguments:
                s += arg.name(True) + '; '

            s = s.strip('; ')
            s += f"] --> {self.return_type.name(True)})"

            return s

        s = "("

        for a in self.arguments:
            s += a.name() + " → "

        s += self.return_type.name() + ')'

        return s


TYPES = [BoolType, NatType, FloatType, StringType, VecType, FuncType]


def replace_type_in_ml(path, type):
    with open(path, 'r') as f:
        gen_lean4 = f.read()

    type_def = re.search(r'let gen_ty(.*)?in', gen_lean4).group(0)
    repl_string = f'let gen_ty = {type} in'
    gen_lean4 = gen_lean4.replace(type_def, repl_string)

    with open(path, 'w') as f:
        f.write(gen_lean4)


def run_and_check(ty, n, size):
    print('Building the executable...')
    with open('gen.lean', 'w') as f:
        subprocess.run(['dune', 'build'],
                       check=True, capture_output=True)
        print('Generating the fuzzer output...')
        subprocess.run(['dune', 'exec', '--', 'gen_lean4', '-n',
                       n, '-size', size], check=True, stdout=f)

    print('Running the typechecker...')
    sp = subprocess.run(
        ['lean', 'gen.lean'], check=True, capture_output=True)

    res_type = re.findall(
        r' : (.*?)(fun|\Z)', sp.stdout.decode('utf-8'), re.DOTALL)

    print('Checking results...')
    for t in res_type:
        clean = re.sub(r'\s+', ' ', t[0].strip(': ').replace('\n', ''))

        clean_noparen = clean.replace('(', '').replace(')', '')
        type_noparen = ty.replace('(', '').replace(')', '')

        if (clean_noparen != type_noparen):
            print('MISMATCH!')
            print(clean_noparen, type_noparen)
            exit(-1)


def argument_parser():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('-n', '--number', default=20)
    argparser.add_argument('-t', '--types', default=10)
    argparser.add_argument('-size', default=1)

    return argparser.parse_args()


def main(args):
    print('Creating target type...')
    for _ in range(int(args.types)):
        target_type = FuncType()
        file_path = os.path.dirname(os.path.realpath(__file__))

        print('Replacing type in the OCaml file...')
        replace_type_in_ml(os.path.join(
            file_path, 'gen_lean4.ml'), target_type.name(True))

        run_and_check(target_type.name()[1:len(
            target_type.name()) - 1], args.number, args.size)
        
        print('Type Finished!')
        
    print('Finished!')


if __name__ == '__main__':
    args = argument_parser()
    main(args)
