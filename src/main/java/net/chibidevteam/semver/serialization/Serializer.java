package net.chibidevteam.semver.serialization;

import static net.chibidevteam.semver.Constants.DELIMITER_META;
import static net.chibidevteam.semver.Constants.DELIMITER_PRE_RELEASE;
import static net.chibidevteam.semver.Constants.SEPARATOR_META;
import static net.chibidevteam.semver.Constants.SEPARATOR_VERSION;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.comparision.Constraint;

public class Serializer {

    private Serializer() {
    }

    public static String serialize(Version version) {
        return serializeVersion(version.getMajor(), version.getMinor(), version.getPatch(), version.getPreRelease(),
                version.getMeta());
    }

    public static String serializeVersion(Integer major, Integer minor, Integer patch, String preRelease,
            List<String> meta) {
        StringBuilder sb = new StringBuilder();
        if (major != null) {
            sb.append(major);
        }
        if (minor != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(minor);
        }
        if (patch != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(patch);
        }
        if (preRelease != null) {
            sb.append(DELIMITER_PRE_RELEASE);
            sb.append(preRelease);
        }
        if (!CollectionUtils.isEmpty(meta)) {
            sb.append(DELIMITER_META);
            sb.append(StringUtils.join(meta, SEPARATOR_META));
        }

        return sb.toString();
    }

    public static String serialize(Constraint constraint) {
        StringBuilder sb = new StringBuilder();

        if (constraint.getConstraintSign() != null && constraint.getVersion() != null) {
            sb.append(constraint.getConstraintSign().getSigns().get(0));
            sb.append(serialize(constraint.getVersion()));
        }

        List<String> ands = new ArrayList<>();
        for (Constraint and : constraint.getAnds()) {
            ands.add(serialize(and));
        }
        if (!ands.isEmpty()) {
            if (sb.length() > 0) {
                sb.append(" ");
            }
            sb.append(StringUtils.join(ands, " "));
        }

        List<String> ors = new ArrayList<>();
        for (Constraint or : constraint.getOrs()) {
            ors.add(serialize(or));
        }
        if (!ors.isEmpty()) {
            if (sb.length() > 0) {
                sb.append(" || ");
            }
            sb.append(StringUtils.join(ors, " || "));
        }

        return sb.toString().trim();
    }
}
