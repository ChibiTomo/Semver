package net.chibidevteam.semver.comparision;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum ConstraintSign {

    EQ("", "="), NOT("!"), LT("<"), LTE("<="), GT(">"), GTE(">="), COMPATIBLE("^"), NEXT_SIGNIFICANT("~");

    private static Map<String, ConstraintSign> map   = new HashMap<>();
    private List<String>                       signs = new ArrayList<>();

    private ConstraintSign(String sign, String... signs) {
        this.signs.add(sign);
        this.signs.addAll(Arrays.asList(signs));
    }

    public List<String> getSigns() {
        return signs;
    }

    public static ConstraintSign getConstaint(String sign) {
        if (map.isEmpty()) {
            for (ConstraintSign val : values()) {
                for (String s : val.signs) {
                    map.put(s, val);
                }
            }
        }
        return map.get(sign);
    }
}
