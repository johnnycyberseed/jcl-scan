package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;

@Service
public class SourceDiscoverer {

  public List<AppSourceFile> discover(List<Path> paths) throws IOException {
    List<AppSourceFile> discovered = new ArrayList<>();
    for (Path path : paths) {
      if (Files.isDirectory(path)) {
        try (var stream = Files.walk(path)) {
          stream
              .filter(Files::isRegularFile)
              .forEach(p -> addIfKnown(discovered, p));
        }
      } else {
        addIfKnown(discovered, path);
      }
    }
    return discovered;
  }

  private void addIfKnown(List<AppSourceFile> out, Path path) {
    try {
      String fileName = path.getFileName().toString();
      AppSourceFile.Kind kind = mapExtensionToKind(fileName);
      if (kind == null) {
        return;
      }
      String content = Files.readString(path, StandardCharsets.UTF_8);
      out.add(new AppSourceFile(fileName, kind, content));
    } catch (IOException e) {
      throw new RuntimeException("Failed reading " + path, e);
    }
  }

  private AppSourceFile.Kind mapExtensionToKind(String fileName) {
    String lower = fileName.toLowerCase();
    if (lower.endsWith(".jcl")) {
      return AppSourceFile.Kind.JCL;
    }
    if (lower.endsWith(".cbl") || lower.endsWith(".cob")) {
      return AppSourceFile.Kind.COBOL;
    }
    if (lower.endsWith(".ezt")) {
      return AppSourceFile.Kind.EASYTRIEVE;
    }
    if (lower.endsWith(".asm") || lower.endsWith(".mac")) {
      return AppSourceFile.Kind.ASSEMBLY;
    }
    return null;
  }
}


