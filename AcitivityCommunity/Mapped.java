
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jimmy
 */
public class Mapped {

    public static void main(String[] args) throws IOException {
        if (args.length == 0) {
            System.err.println("Requires a map file.");
            System.exit(1);
        }
        String modularity = "?";
        Map<String, String> map = new HashMap<>();
        Map<String, Integer> communitySize = new HashMap<>();
        {
            File mapFile = new File(args[0]);
            BufferedReader in = new BufferedReader(new FileReader(mapFile));
            String line;
            while ((line = in.readLine()) != null) {
                if (line.startsWith("#")) {
                    String modularityString = "# Modularity: ";
                    if (line.startsWith(modularityString)) {
                        modularity = line.substring(modularityString.length());
                    } else {
                        continue;
                    }
                }
                int index = line.indexOf(' ');
                String key = line.substring(0, index);
                String value = line.substring(index + 1);
                if (map.put(key, value) != null) {
                    System.err.println("Duplicate key \"" + key + "\" in the map file.");
                    System.exit(1);
                }
                Integer size = communitySize.get(value);
                if (size == null) {
                    size = 0;
                }
                communitySize.put(value, size + 1);
            }
        }

        long same = 0;
        long total = 0;

        String last = "";

        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = in.readLine()) != null) {
            String value = map.get(line);
            if (value == null) {
                System.err.println("No match for key \"" + line + "\".");
                System.exit(1);
            }
            System.out.println(line + " " + value + " (" + communitySize.get(value) + ")");
            if (value.equals(last)) {
                same++;
            }
            total++;
            last = value;
        }

        System.out.println("#Nodes: " + map.size());
        System.out.println("#Modularity: " + modularity);
        System.out.println("#Communities: " + communitySize.size());
        int largest = Collections.max(communitySize.values());
        System.out.println("#Largest Communities: " + largest + " (" + (largest * 100 / map.size()) + "%)");
        System.out.println("#Stayed within community: " + same + "/" + total
                + " (" + (same * 100 / total) + "%)");
    }
}
