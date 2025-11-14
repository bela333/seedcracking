# This program finds the world seed from a chunk seed
import z3

chunkx = 7
chunkz = 16

seedsort = z3.BitVecSort(48)
intsort = z3.BitVecSort(32)

multiplier = 0x5DEECE66D
addend = 0xB
mask = (1 << 48) - 1

solver = z3.Solver()
bits = "011011111111101011010110000001000111001110110011"
known = len(bits)
unknown = 48 - known
seed = (z3.SignExt(64 - unknown, z3.BitVec("seed", unknown)) << known) | int(bits, 2)
# seed = z3.SignExt(16, z3.BitVecVal(190859500651332, 48))
seed0 = seed ^ multiplier
seed1 = (seed0 * multiplier) + addend
seed2 = (seed1 * multiplier) + addend
seed3 = (seed2 * multiplier) + addend
seed4 = (seed3 * multiplier) + addend


def long_to_int_to_long(n):
    return z3.SignExt(32, z3.Extract(31, 0, n))


# randx = (
#     ((z3.LShR(seed1, 16) << 32) + long_to_int_to_long(z3.LShR(seed2, 16))) / 2
# ) * 2 + 1
# randz = (
#     ((z3.LShR(seed3, 16) << 32) + long_to_int_to_long(z3.LShR(seed4, 16))) / 2
# ) * 2 + 1


randx = ((z3.LShR(seed1, 16) << 32) + long_to_int_to_long(z3.LShR(seed2, 16))) | 1
randz = ((z3.LShR(seed3, 16) << 32) + long_to_int_to_long(z3.LShR(seed4, 16))) | 1

chunkseed = z3.Extract(47, 0, chunkx * randx + (chunkz * randz) ^ seed)

# goal = z3.Then("simplify", z3.With("bit-blast", blast_full=True))(
#     z3.simplify(chunkseed) == 112574147696699
# ).as_expr()

# 0b101001000101001
bit = 31
goal = z3.Then("simplify", "bit-blast")(z3.Extract(bit, bit, chunkseed) == 1).as_expr()
print(goal)


# print(goal)
# solver.add(goal)
# print(solver.check())
# print(solver.model())
# print(solver.model().decls())


# solution = solver.model()

# print(hex(solution.as_signed_long()))
# print(solution.as_signed_long())
# print(solution.as_long())
