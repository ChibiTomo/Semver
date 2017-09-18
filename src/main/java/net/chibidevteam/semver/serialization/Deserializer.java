package net.chibidevteam.semver.serialization;

import static net.chibidevteam.semver.Constants.ESCAPING_STR;
import static net.chibidevteam.semver.Constants.SEPARATOR_META;
import static net.chibidevteam.semver.Constants.VERSION_PATTERN;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import lombok.extern.slf4j.Slf4j;
import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.SemverHelper;
import net.chibidevteam.semver.VersionPart;
import net.chibidevteam.semver.comparision.Constraint;
import net.chibidevteam.semver.comparision.ConstraintSign;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

@Slf4j
public class Deserializer {

    private static final Pattern SPLITTER_PATTERN = Pattern.compile("^(?<SIGN>[^\\w]{0,2})(?<VERSION>\\d.*)$");

    private Deserializer() {
    }

    public static Constraint deserializeConstraint(String constraint) throws MalformedVersionException {
        Constraint result = new Constraint();

        List<List<Constraint>> orList = new ArrayList<>();

        String[] orArray = constraint.split("\\|\\|");
        for (String or : orArray) {
            List<Constraint> andList = new ArrayList<>();
            orList.add(andList);
            String[] andArray = or.trim().split(" ");
            for (String and : andArray) {
                Constraint andConstraint = new Constraint(getConstraintSign(and), getVersion(and));
                andList.add(andConstraint);
            }
        }

        if (orList.size() == 1) {
            if (orList.get(0).size() == 1) {
                return orList.get(0).get(0);
            } else {
                for (Constraint c : orList.get(0)) {
                    result.addAnd(c);
                }
                return result;
            }
        }

        for (List<Constraint> ands : orList) {
            Constraint or = new Constraint();
            if (ands.size() == 1) {
                or = ands.get(0);
            } else {
                for (Constraint and : ands) {
                    or.addAnd(and);
                }
            }
            result.addOr(or);
        }

        return result;
    }

    public static ConstraintSign getConstraintSign(String constraint) {
        Matcher m = SPLITTER_PATTERN.matcher(constraint);
        return m.matches() ? ConstraintSign.getConstaint(m.group("SIGN")) : null;
    }

    public static Version getVersion(String constraint) throws MalformedVersionException {
        Matcher m = SPLITTER_PATTERN.matcher(constraint);
        return m.matches() ? SemverHelper.deserializeVersion(m.group("VERSION")) : null;
    }

    public static Version deserializeVersion(String str) throws MalformedVersionException {
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
