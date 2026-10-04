unit module ExportedSubsetWhen;
subset Bin of Buf is export;
subset Small of Int is export where * < 10;
