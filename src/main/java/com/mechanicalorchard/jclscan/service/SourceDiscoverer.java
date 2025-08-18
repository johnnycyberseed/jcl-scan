package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;

@Slf4j
@Service
public class SourceDiscoverer {

  public List<AppSourceFile> discover(List<Path> paths) throws IOException {
    log.info("Discovering sources in {}", paths);
    List<AppSourceFile> discovered = new ArrayList<>();
    for (Path path : paths) {
      String raw = path.toString();
      if (raw.startsWith("classpath:")) {
        String cpRoot = raw.substring("classpath:".length());
        discoverFromClasspath(discovered, cpRoot);
      } else if (Files.isDirectory(path)) {
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

  private void discoverFromClasspath(List<AppSourceFile> out, String classpathRoot) throws IOException {
    ClassLoader cl = Thread.currentThread().getContextClassLoader();
    var url = cl.getResource(classpathRoot);
    if (url == null) {
      log.warn("Classpath root '{}' not found", classpathRoot);
      return;
    }
    try {
      Path rootPath = Path.of(url.toURI());
      try (var stream = Files.walk(rootPath)) {
        List<Path> files = stream
            .filter(Files::isRegularFile)
            .collect(Collectors.toList());

        files.stream()
            .sorted(Comparator.comparing((Path p) -> p.getFileName().toString().toLowerCase()).reversed())
            .forEach(p -> addIfKnown(out, p));
      }
    } catch (Exception e) {
      throw new IOException("Failed to read classpath resources under '" + classpathRoot + "'", e);
    }
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
