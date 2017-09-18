package net.chibidevteam.semver;

import java.util.Comparator;
import java.util.regex.Pattern;

class Constants {

    private Constants() {
    }

    public static final Comparator<Integer> INTEGER_COMPARATOR    = Comparator.nullsFirst(Integer::compareTo);
    public static final Comparator<String>  STRING_COMPARATOR     = Comparator.nullsLast(String::compareTo);

    public static final String              SEPARATOR_VERSION     = ".";
    public static final String              DELIMITER_PRE_RELEASE = "-";
    public static final String              DELIMITER_META        = "+";
    public static final String              SEPARATOR_META        = ".";

    public static final String              ESCAPING_STR          = "\\";

    public static final Pattern             VERSION_PATTERN       = Pattern.compile(
            "^(?<MAJOR>\\d+)(?:\\.(?<MINOR>\\d+)(?:\\.(?<PATCH>\\d+))?)?(?:-(?<PRERELEASE>[0-9A-Za-z-]+))?(?:\\+(?<META>[0-9A-Za-z-.]+))?$");

}
