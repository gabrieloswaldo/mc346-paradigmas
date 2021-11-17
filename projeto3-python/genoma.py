# MC346 - Projeto 3 - Python
#
# Alunos:
#   Gabriel Henrique Rosa Oswaldo - 172185
#   Bruno Rosano Laporte Ambrosio - 195141

import fileinput
from typing import List


def show_sequences(sequences: List[str]) -> None:
    """
    Print each sequence.
    """
    for sequence in sequences:
        print(sequence)


def update_longest_seq(sequences: List[str], new_seq: str) -> None:
    """
    Update a sequence in the list of sequences, if the new sequence
    is a longest version of the sequence. If it doesn't match any 
    sequence, add the new sequence to list.
    """
    for i, seq in enumerate(sequences):
        if new_seq.find(seq) != -1:
            sequences[i] = new_seq
            return
    sequences.append(new_seq)


def concat(str1: str, str2: str, size: int) -> str:
    """
    Concatenate two strings that have the same substring (with length 'size')
    at the end of 'str1' and the beginning of 'str2'.
    """
    return str1[:-size] + str2


def substring(str1: str, str2: str) -> str:
    """
    Return the substring that matches the end of 'str1' and the 
    beginning of 'str2', and this substring length is greater 
    than or equal to 4. Return None, otherwise.
    """
    for i in range(len(str1)):
        sub = str1[i:]
        if str2.startswith(sub) and len(sub) >= 4:
            return sub
    return None


def get_genome_sequences(patches: List[str]) -> list:
    """
    Return the genome possible sequences with common genes, 
    given the genome patches.
    """
    sequences = patches[:]
    patches_to_visit = {patch: i for i, patch in enumerate(patches)}

    while bool(patches_to_visit):
        patch1 = patches_to_visit.popitem()[0]

        patches_copy = sequences[:]
        if patch1 in patches_copy:
            patches_copy.remove(patch1)

        for patch2 in patches_copy:
            sub = substring(patch1, patch2)
            if sub is not None:
                sequence = concat(patch1, patch2, len(sub))
                sequences.remove(patch2)
                sequences[sequences.index(patch1)] = sequence
                update_longest_seq(sequences, sequence)
                patches_to_visit[sequence] = len(patches_to_visit)

    return sequences


if __name__ == "__main__":

    patches = []
    for line in fileinput.input():
        patches.append(line.rstrip("\n"))
    show_sequences(get_genome_sequences(patches))
