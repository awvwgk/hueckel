# Hückel eigenvalue solver
This code takes a Hückel matrix and solves the Hückel
eigenvalue problem, then returning energy levels in
the Hückel unit (β) as well as Hückel molecular orbitals.

## How to use?

run the program without an argument for interactive mode

    s-h

then enter the number of atoms ($nat) in the π-system and the
electrons ($nel) behind the shown promt. In the $hmat block you can
specify the non-zero elements of the Hückel matrix.

The input block can be copied into a file and read in to rerun the
calculation by

    s-h input-file

For more information run

    s-h -h
