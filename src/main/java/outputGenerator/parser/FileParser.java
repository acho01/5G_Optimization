package outputGenerator.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class FileParser {
    public static List<Entry> getListFromFilePath(String filePath) throws IOException {
        Path firstPath = Path.of(filePath);
        String file_1 = Files.readString(firstPath);

        return getListOfEntriesFromOutputFile(file_1);
    }


    public static List<Entry> getListOfEntriesFromOutputFile(String fileContent){
        String[] array = fileContent.split("\n");
        return Arrays.stream(array)
                .map(FileParser::getEntryFromOneRowOfOutput)
                .collect(Collectors.toList());
    }

    private static Entry getEntryFromOneRowOfOutput(String row) {
        List<Double> parameters = parseRow(row);
        return new Entry(parameters.get(0), parameters.get(2), parameters.get(1));
    }


    private static List<Double> parseRow(String row) {
        return Arrays.asList(row.split(" "))
                .stream()
                .filter(s -> !s.isBlank())
                .map(Double::parseDouble)
                .collect(Collectors.toList());
    }
}
