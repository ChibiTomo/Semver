package net.chibidevteam.semver;

import static net.chibidevteam.semver.Constants.ESCAPING_STR;
import static net.chibidevteam.semver.Constants.SEPARATOR_META;
import static net.chibidevteam.semver.Constants.VERSION_PATTERN;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;

import lombok.extern.slf4j.Slf4j;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

@Slf4j
class Deserializer {

    private Deserializer() {
    }

    public static Version deserialize(String str) throws MalformedVersionException {
        if (log.isDebugEnabled()) {
            log.debug("Parsing given version: {}", str);
        }

        Matcher m = VERSION_PATTERN.matcher(str);
        if (!m.matches()) {
            throw new MalformedVersionException("Version '" + str + "' does not match Semmentic Version format");
        }

        try {
            return new Version(getMajor(m), getMinor(m), getPatch(m), getPreRelease(m), getMeta(m));
        } catch (NumberFormatException e) {
            throw new MalformedVersionException("MAJOR, MINOR and/or PATCH part is malformed: '" + str + "'", e);
        }
    }

    private static Integer getMajor(Matcher m) {
        String str = m.group(VersionPart.MAJOR.toString());
        return Integer.parseInt(str);
    }

    private static Integer getMinor(Matcher m) {
        String str = m.group(VersionPart.MINOR.toString());
        return !StringUtils.isBlank(str) ? Integer.parseInt(str) : null;
    }

    private static Integer getPatch(Matcher m) {
        String str = m.group(VersionPart.PATCH.toString());
        return !StringUtils.isBlank(str) ? Integer.parseInt(str) : null;
    }

    private static String getPreRelease(Matcher m) {
        return m.group(VersionPart.PRERELEASE.toString());
    }

    private static List<String> getMeta(Matcher m) {
        String str = m.group(VersionPart.META.toString());
        return str != null ? Arrays.asList(str.split(ESCAPING_STR + SEPARATOR_META)) : null;
    }
}
