# PRJ4TAX - Tax Calculation Batch Program
Simulated IRS Tax Proccessing batch program. Processes a sequential batch file to determine the taxes owed by each user, if they are getting a refund, and if they need to be flagged for an audit. Checks VSAM DB to see historical tax history and update it.
Incorporates Jay Moseley's VSAMIO assembler routine and code snippets to access the VSAM file.

COMPILES ON MVS 3.8 TK5 - FITS COBOL 68 STANDARD

You will need to have the following folders/files created:
- PRJ4.DEV.BCOB - for source files
- PRJ4.DEV.JCL  - for jcl files
- PRJ4.DEV.COPYBOOk - for copybooks
- SYS2.VSAMIO.OBJECT - for vsamio assembler binary
- SYS2.VSAMIO.SOURCE - for vsamio source files
(NOTE: these folders will be automatically created if you use the x2 main setup files)


## How to setup:
- copy the 1-SETUP-VSAMIO.jcl to your mainframe under HERC01 and run it, it will create the vsamio files and copile the vsamio binary
- copy the 2-SETUP-PRJ4-FILES.jcl to your mainframe under HERC01 and run it, it will setup the files for PRJ4
- go to the PRJ4.DEV.JCL(S2FILES) and submit it to create your datasets, your vsam file, and load the master record into your vsam
- go to the PRJ4.DEV.JCL(S3RUN) and submit it to compile, link and run your MYTAX cobol program
- you can see the display statements using 3.8, and it should create output in your local PRJ4 view, just a back out and search it again

## Code:
- If you want to just view the code that went into this in a more seemless manner, you can dig into the CODE folder to each component part

## Sources:
- Module 8.2: COBOL Subprogram Practical | COBOL Programming Full Course https://youtu.be/zSVew6CQkO0
- Jay Moseley's VSAMIO Utility: www.jaymoseley.com/hercules/vsam_io/vscobol.htm