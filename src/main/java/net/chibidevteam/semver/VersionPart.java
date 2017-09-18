package net.chibidevteam.semver;

import lombok.Getter;

@Getter
public enum VersionPart {
    MAJOR, MINOR, PATCH, PRERELEASE, META;
}
