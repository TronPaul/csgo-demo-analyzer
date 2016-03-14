package csgo_demo_reader;

public class PropSorter {
    public static void sortProperties(Property[] properties, int[] priorities) {
        int start = 0;
        for (int priority : priorities) {
            while(true) {
                int currentProperty = start;
                while(currentProperty < properties.length) {
                    Property prop = properties[currentProperty];
                    if (prop.getPriority() == priority || (priority == 64 && prop.isChangesOften())) {
                        if (start != currentProperty) {
                            Property temp = properties[start];
                            properties[start] = properties[currentProperty];
                            properties[currentProperty] = temp;
                        }
                        start++;
                        break;
                    }
                    currentProperty++;
                }
                if (currentProperty == properties.length) {
                    break;
                }
            }
        }
    }
}
