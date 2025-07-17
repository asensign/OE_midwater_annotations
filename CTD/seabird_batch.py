from seabird_processing import Batch, configs

xmlcon = './path/to/xmlcon/12-3456.xmlcon'

# Create a pipeline with some config files
batch = Batch([
    configs.DatCnvConfig(
        # `output_file_suffix` is optional
        output_dir="./datcnv", output_file_suffix="_datcnv",
        xmlcon=xmlcon, psa='./path/to/DatCnv.psa'),
    configs.FilterConfig(
        output_dir="./filter", output_file_suffix="_filter",
        xmlcon=xmlcon, psa='./path/to/Filter.psa'),
    configs.AlignCTDConfig(
        output_dir="./alignctd", output_file_suffix="_alignctd",
        xmlcon=xmlcon, psa='./path/to/AlignCTD.psa'),
    configs.CellTMConfig(
        output_dir="./celltm", output_file_suffix="_celltm",
        xmlcon=xmlcon, psa='./path/to/CellTM.psa'),
    configs.LoopEditConfig(
        output_dir="./loopedit", output_file_suffix="_loopedit",
        xmlcon=xmlcon, psa='./path/to/LoopEdit.psa'),
    configs.DeriveConfig(
        output_dir="./derive", output_file_suffix="_derive",
        xmlcon=xmlcon, psa='./path/to/Derive.psa'),
    configs.DeriveTEOS10Config(
        output_dir="./deriveteos10", output_file_suffix="_deriveteos10",
        xmlcon=xmlcon, psa='./path/to/DeriveTEOS_10.psa'),
    configs.BinAvgConfig(
        output_dir="./binavg", output_file_suffix="_binavg",
        xmlcon=xmlcon, psa='./path/to/BinAvg.psa'),
])

batch.run("./*.hex")

# You may also run an individual Config object
converter = configs.DatCnvConfig(
    output_dir="./datcnv", output_file_suffix="_datcnv",
    xmlcon=xmlcon, psa='./path/to/DatCnv.psa'
)
converter.run("./some/file.hex")
